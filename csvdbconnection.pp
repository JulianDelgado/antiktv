unit CsvDbConnection;

{*
Virtual CSV Database connection, usable for 3-tier appliacations, something like SOAP or RPC, but use csv format
hikikomori82 at gmail dot com

History:
2008/09/22 - ripoff from oracle version (because it is smallest, 16 kB), successfully compiled
2008/09/24 - first sample data retrieved (table 2x2 of float 7.65)
2008/09/25 - added CsvSplit function, added Statement and Cache to cursor, initial work on :parameters
2008/09/26 - basic :parameters parsing

Examples:

  Request1:
  
method,nationality,activity,city
getUsers,US,both,Boston

  Response1: 

i_user,v_name,i_age,j_gender,b_active
integer,varchar(50),integer,integer,boolean
1,John Smith,20,1,true
2,Jack Brows,23,1,true
3,Mary White,21,2,true
4,Bill Gates,90,1,false

  Request2:
  
method,id
deleteUser,4

  Response2:
  
rowsAffected
1

  Request3:
  
method,id,name
updateUser,2,Jack Brown

--- or?

  Request1b:
  
    getUsers,nationality,US,activity,both,city,boston
  
  or 
    
    getUsers,nationality=US,activity=both,city=boston


}

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils, sqldb, db, dbconst;
(* {$IfDef LinkDynamically}
  ocidyn,
{$ELSE}
  oci,
{$ENDIF}
  oratypes; *)

const  
  SQLT_INT = 3;   { integer  }
  SQLT_FLT = 4;   { floating point number  }
  SQLT_STR = 5;   { zero terminated string  }
  SQLT_DAT = 12;  { date in oracle format  }
																				       

type
  TCsvDbTrans = Class(TSQLHandle)
    protected
  end;
  
  TCsvDbFieldBuf = record
    Buffer : pointer;
    Ind    : smallint;
  end;

  TCsvDbCursor = Class(TSQLCursor)
    protected
      //FOciStmt     : POCIStmt;
      FieldBuffers : array of TCsvDbFieldBuf;
      ParamBuffers : array of TCsvDbFieldBuf;
      // testing data
      Statement : TStringList;	// contain request (equivalent to select)
      Cache : TStringList;	// contain fetched data
    public
      constructor Create; virtual;
      destructor Destroy; override;
    end;

  { TCsvDbConnection }

  TCsvDbConnection = class (TSQLConnection)
  private
    //FOciEnvironment : POciEnv;
    //FOciError       : POCIError;
    //FOciSvcCtx      : POCISvcCtx;
    FUserMem        : pointer;
    procedure HandleError;
    procedure SetParameters(cursor : TSQLCursor;AParams : TParams);
  protected
    // - Connect/disconnect
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    // - Handle (de)allocation
    function AllocateCursorHandle:TSQLCursor; override;
    procedure DeAllocateCursorHandle(var cursor:TSQLCursor); override;
    function AllocateTransactionHandle:TSQLHandle; override;
    // - Statement handling
    procedure PrepareStatement(cursor:TSQLCursor; ATransaction:TSQLTransaction; buf:string; AParams:TParams); override;
    procedure UnPrepareStatement(cursor:TSQLCursor); override;
    // - Transaction handling
    function GetTransactionHandle(trans:TSQLHandle):pointer; override;
    function StartDBTransaction(trans:TSQLHandle; AParams:string):boolean; override;
    function Commit(trans:TSQLHandle):boolean; override;
    function Rollback(trans:TSQLHandle):boolean; override;
    procedure CommitRetaining(trans:TSQLHandle); override;
    procedure RollbackRetaining(trans:TSQLHandle); override;
    // - Statement execution
    procedure Execute(cursor:TSQLCursor; ATransaction:TSQLTransaction; AParams:TParams); override;
    // - Result retrieving
    procedure AddFieldDefs(cursor:TSQLCursor; FieldDefs:TFieldDefs); override;
    function Fetch(cursor:TSQLCursor):boolean; override;
    function LoadField(cursor:TSQLCursor; FieldDef:TFieldDef; buffer:pointer; out CreateBlob : boolean):boolean; override;
//    function CreateBlobStream(Field:TField; Mode:TBlobStreamMode):TStream; override;
    procedure FreeFldBuffers(cursor:TSQLCursor); override;

  public
    constructor Create(AOwner : TComponent); override;
  end;

  TCsvDbConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;

implementation

uses math;

{*    
Split string in csv format into pieces separated by colon
}
function CsvSplit(ASrc : string; ADst : TStrings) : boolean;
var i,len,k : integer;
    quote,innerquote : boolean;
    s,ns : string;
    c,oldc,u : char;
begin
  ADst.Clear;
  i := 1;
  quote := false;
  innerquote := false;
  s := '';
  oldc := '-';
  len := length(ASrc);
  while (i<=len) do
  begin
    c := ASrc[i];
    if i=len then
      s := s + c;
    if (c='"')and(oldc='"') then
      innerquote := true;
    if (not quote and (c=','))or(i=len) then
    begin
      // if there are quotes inside, convert them "" --> "
      ns := '';
      u := '-';
      if innerquote then
      begin
        for k := 2 to length(s)-1 do 	// ommit outer "
	begin
	  if (s[k]='"') and (u='"') then
	  begin
	    //ns := ns + 'Q';
	    continue;
	  end;
	  ns := ns + s[k];
	  u := s[k];
	end;
        ADst.Add(ns);
      end else
        ADst.Add(s);
      //writeln('s=__',s,'__ ns=__',ns,'__ i=',i,' len=',len,' iq=',innerquote);
      s := '';
      innerquote := false;
    end else
      s := s + c;
    if c='"' then
      quote := not quote;
    inc(i);
    oldc := c;
  end;
  result := ADst.Count > 0;
end;

{*
Fill all :ALIAS parameters with corresponding values
}
procedure FillParams(AStatement : string; AParams : TParams);
var i,len : integer;
    quote,inalias : boolean;
    s,alias : string;
    p : TParam;
begin
  writeln('FillParams(__',AStatement,'__, ',AParams.Count,' params)');
  len := length(AStatement);
  quote := false;
  i := 1;
  s := '';
  alias := '';
  inalias := false;
  while i<=len do
  begin
    if not quote then
    begin
      // end of single alias?
      if inalias and (not (AStatement[i] in ['a'..'z','A'..'Z','0'..'9'])) or (i=len) then
      begin
        if (i=len) then
	begin
	  if AStatement[i] <> ')' then
  	    alias := alias + AStatement[i];
	end else
          inalias := false;
	// replace parameter by value
	if AParams<>nil then
	begin
	  writeln('  hladam param ',alias);
	  p := AParams.ParamByName(alias);
	  if p <> nil then
	  begin
	    // handle data types
            case p.DataType of
              ftInteger,ftFloat : s := s + p.AsString;
              ftString,ftDate 	: s := s + '"'+p.AsString+'"';
              ftBoolean 	: s := s + p.AsString;
	    else
	      s := s + '<unsupported data type>';
	    end;
	  end else
    	    s := s + '['+alias+']';
	end else
  	  s := s + '('+alias+')';
	writeln('alias=__',alias,'__');
	alias := '';
      end;
      // in alias detect it's name, outside alias cut of real "non-alias" characters
      if inalias then
        alias := alias + AStatement[i]
      else 
        if AStatement[i] <> ':' then
          s := s + AStatement[i];
      // begin of alias
      if AStatement[i]=':' then
        inalias := true;
    end;
    if AStatement[i]='"' then quote := not quote;
    inc(i);
  end;
  writeln('  final SQL is = __',s,'__');
end;    


ResourceString
  SErrEnvCreateFailed = 'The creation of an Oracle environment failed.';
  SErrHandleAllocFailed = 'The allocation of the error handle failed.';
  SErrOracle = 'Oracle returned error %s:';


constructor TCsvDbCursor.Create;
begin
  inherited;
  Statement := TStringList.Create;
  Cache := TStringList.Create;
end;

destructor TCsvDbCursor.Destroy;
begin
  Statement.Free;
  Cache.Free;
  inherited;
end;

procedure TCsvDbConnection.HandleError;
{var errcode : sb4;
    buf     : array[0..1023] of char;}
begin
  writeln(ClassName+'.HandleError');
  {OCIErrorGet(FOciError,1,nil,errcode,@buf[1],1023,OCI_HTYPE_ERROR);}
  DatabaseErrorFmt('Unknown TCsvDbConnection error, LE='+LineEnding+' (buf)',[inttostr(1{errcode})],self);
end;

procedure TCsvDbConnection.DoInternalConnect;
{var ConnectString : string;}
begin
  writeln(ClassName+'.DoInternalConnect');
(*
{$IfDef LinkDynamically}
  InitialiseOCI;
{$EndIf} *)
  inherited DoInternalConnect;
  writeln(ClassName+'.DoInternalConnect hostname=',hostname,' databasename=',databasename);
(*  FUserMem := nil;
  if OCIEnvCreate(FOciEnvironment,oci_default,nil,nil,nil,nil,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrEnvCreateFailed,self);

  if OciHandleAlloc(FOciEnvironment,FOciError,OCI_HTYPE_ERROR,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);

  if hostname='' then connectstring := databasename
  else connectstring := '//'+hostname+'/'+databasename;

  if OCILogon2(FOciEnvironment,FOciError,FOciSvcCtx,@username[1],length(username),@password[1],length(password),@connectstring[1],length(connectstring),OCI_DEFAULT) = OCI_ERROR then
    HandleError;*)
end;

procedure TCsvDbConnection.DoInternalDisconnect;
begin
  writeln(ClassName+'.DoInternalDisconnect');
  inherited DoInternalDisconnect;
  writeln(ClassName+'.DoInternalDisconnect');
(*
  if OCILogoff(FOciSvcCtx,FOciError)<> OCI_SUCCESS then
    HandleError;

  OCIHandleFree(FOciSvcCtx,OCI_HTYPE_SVCCTX);
  OCIHandleFree(FOciError,OCI_HTYPE_ERROR);

  OCIHandleFree(FOciEnvironment,OCI_HTYPE_ENV);
{$IfDef LinkDynamically}
  ReleaseOCI;
{$EndIf}
*)
end;

function TCsvDbConnection.AllocateCursorHandle: TSQLCursor;
var Cursor : TCsvDbCursor;
begin
  writeln(ClassName+'.AllocateCursorHandle');
  Cursor:=TCsvDbCursor.Create;
  //OciHandleAlloc(FOciEnvironment,Cursor.FOciStmt,OCI_HTYPE_STMT,0,FUserMem);
  Result := Cursor;
end;

procedure TCsvDbConnection.DeAllocateCursorHandle(var cursor: TSQLCursor);
var tel : word;
begin
  writeln(ClassName+'.DeAllocateCursorHandle');
  with cursor as TCsvDbCursor do
  begin
    //OCIHandleFree(FOciStmt,OCI_HTYPE_ERROR);
    if Length(FieldBuffers) > 0 then
      for tel := 0 to high(FieldBuffers) do freemem(FieldBuffers[tel].buffer);
  end;
  FreeAndNil(Cursor);
end;

function TCsvDbConnection.AllocateTransactionHandle: TSQLHandle;
begin
  writeln(ClassName+'.AllocateTransactionHandle');
  Result:=nil;
end;

procedure TCsvDbConnection.PrepareStatement(Cursor: TSQLCursor;
  ATransaction: TSQLTransaction; buf: string; AParams: TParams);
  
var tel      : integer;
    OFieldSize : longint;
    OFieldType : word;
(*    FOcibind : POCIDefine;
    
    OFieldType   : ub2;
    OFieldSize   : sb4;
*)
    i : integer;
    quote : boolean;
    
begin
  // this is called in Param.ParamByName('asdf').AsString := 'asdfasdfasdfa';
  writeln(ClassName,'.PrepareStatement (buf="',buf,'", count=',AParams.Count,')');

  FillParams(buf,AParams);
  
  for i := 0 to AParams.Count-1 do
  begin
  
    
  end;
  
  
  with Cursor as TCsvDbCursor do
  begin
    // remember statement (unprepared)
    Statement.Text := buf;
    
    // fill all :ALIAS parameters with corresponding values
{    quote := false;
    for i := 0 to length(buf) do
    begin
      if not quote then
      begin
        if buf[i]=':' then
	
      end;
      if buf[i]='"' then quote := not quote;
    end;}
    
    
    //if OCIStmtPrepare(FOciStmt,FOciError,@buf[1],length(buf),OCI_NTV_SYNTAX,OCI_DEFAULT) = OCI_ERROR then
    //  HandleError;
    if assigned(AParams) then
    begin
      SetLength(ParamBuffers,AParams.Count);
      for tel := 0 to AParams.Count-1 do
        begin

        case AParams[tel].DataType of
          ftInteger : begin OFieldType := SQLT_INT; OFieldSize := sizeof(integer); end;
          ftFloat : begin OFieldType := SQLT_FLT; OFieldSize := sizeof(double); end;
          ftDate, ftDateTime : begin OFieldType := SQLT_DAT; OFieldSize := 7; end;
          ftString  : begin OFieldType := SQLT_STR; OFieldSize := 4000; end;

        end;
        parambuffers[tel].buffer := getmem(OFieldSize);

	// debug
	writeln('  AParams[tel=',tel,'].Name = ',AParams[tel].Name,' value=',AParams[tel].Value,' oft=',OFieldType);

	// toto nejak zaonacit - asi nieco ako parsovanie "select * from user where name = :NAME and age = :AGE" a dosadzovanie parametrov
	
//        FOciBind := nil;

//        if OCIBindByName(FOciStmt,FOcibind,FOciError,pchar(AParams[tel].Name),length(AParams[tel].Name),ParamBuffers[tel].buffer,OFieldSize,OFieldType,@ParamBuffers[tel].ind,nil,nil,0,nil,OCI_DEFAULT )= OCI_ERROR then
//          HandleError;

        end;
      end;
    end;
end;

procedure TCsvDbConnection.SetParameters(cursor : TSQLCursor;AParams : TParams);
var SQLVarNr       : integer;
    i              : integer;
    f              : double;
    year,month,day : word;
    //db             : array[0..4] of byte;
    pb             : pbyte;
    s              : string;
begin
  writeln(ClassName+'.SetParameters');
  with cursor as TCsvDbCursor do 
    for SQLVarNr := 0 to High(ParamBuffers) do 
      with AParams[SQLVarNr] do
      begin
        if IsNull then 
	  parambuffers[SQLVarNr].ind := -1 
	else
          parambuffers[SQLVarNr].ind := 0;
        case DataType of
          ftInteger: 
	    begin
              i := asInteger;
              move(i,parambuffers[SQLVarNr].buffer^,sizeof(integer));
            end;
          ftFloat: 
	    begin
              f := asFloat;
              move(f,parambuffers[SQLVarNr].buffer^,sizeof(double));
            end;
          ftString:
	    begin
              s := asString+#0;
              move(s[1],parambuffers[SQLVarNr].buffer^,length(s)+1);
            end;
          ftDate, ftDateTime: 
	    begin
              DecodeDate(asDateTime,year,month,day);
              pb := parambuffers[SQLVarNr].buffer;
              pb[0] := (year div 100)+100;
              pb[1] := (year mod 100)+100;
              pb[2] := month;
              pb[3] := day;
              pb[4] := 1;
              pb[5] := 1;
              pb[6] := 1;
            end;
         end;
      end;
end;

procedure TCsvDbConnection.UnPrepareStatement(cursor: TSQLCursor);
begin
  writeln(ClassName+'.UnPrepareStatement');
//
end;

function TCsvDbConnection.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
// Transactions not implemented yet
    result := nil;
end;

function TCsvDbConnection.StartDBTransaction(trans: TSQLHandle; AParams: string): boolean;
begin
  writeln(ClassName+'.StartDbTransaction');
// Transactions not implemented yet
  result := false;
end;

function TCsvDbConnection.Commit(trans: TSQLHandle): boolean;
begin
  writeln(ClassName+'.Commit');
// Transactions not implemented yet
  result := false;
end;

function TCsvDbConnection.Rollback(trans: TSQLHandle): boolean;
begin
  writeln(ClassName+'.Rollback');
// Transactions not implemented yet
  result := false;
end;

procedure TCsvDbConnection.CommitRetaining(trans: TSQLHandle);
begin
  writeln(ClassName+'.CommitRetaining');
// Transactions not implemented yet
end;

procedure TCsvDbConnection.RollbackRetaining(trans: TSQLHandle);
begin
  writeln(ClassName+'.RollbackRetaining');
// Transactions not implemented yet
end;

procedure TCsvDbConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
begin
  writeln(ClassName+'.Execute');
  if Assigned(APArams) and (AParams.count > 0) then SetParameters(cursor, AParams);
  if cursor.FStatementType = stSelect then
    begin
      writeln('  1 ...');
      //if OCIStmtExecute(FOciSvcCtx,(cursor as TOracleCursor).FOciStmt,FOciError,0,0,nil,nil,OCI_DEFAULT) = OCI_ERROR then
        //HandleError;
    end else
    begin
      writeln('  ...');
    //if OCIStmtExecute(FOciSvcCtx,(cursor as TOracleCursor).FOciStmt,FOciError,1,0,nil,nil,OCI_DEFAULT) = OCI_ERROR then
     // HandleError;
    end;
end;

procedure TCsvDbConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);

//var Param      : POCIParam;
var    tel        : longword; //ub4;

    FieldType  : TFieldType;
    FieldName  : string;
    FieldSize  : word;

    NumCols      : longword;

    OFieldType   : word;
//    OFieldName   : Pchar;
    OFieldSize   : longint;
//    OFNameLength : longword;
//    FOciDefine   : POCIDefine;
//    OPrecision   : integer;
//    OScale       : byte;

begin
  writeln(ClassName,'.AddFieldDefs ...');
  //Param := nil;
  with cursor as TCsvDbCursor do
  begin
    //if OCIAttrGet(FOciStmt,OCI_HTYPE_STMT,@numcols,nil,OCI_ATTR_PARAM_COUNT,FOciError) = OCI_ERROR then
    //  HandleError;
    numcols := 2;

    // Let op, moet gewist worden. En in een keer gealloceerd
    Setlength(FieldBuffers,numcols);
    writeln('  numcols=',numcols);

    for tel := 1 to numcols do
      begin
      
      
      //if OCIParamGet(FOciStmt,OCI_HTYPE_STMT,FOciError,Param,tel) = OCI_ERROR then
        //HandleError;

      //if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldType,nil,OCI_ATTR_DATA_TYPE,FOciError) = OCI_ERROR then
        //HandleError;

      //if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldSize,nil,OCI_ATTR_DATA_SIZE,FOciError) = OCI_ERROR then
        //HandleError;

      FieldSize := 0;

	// zatial napevno float budeme vracat
      FieldType := ftFloat;
      OFieldType := SQLT_FLT;
      OFieldSize:=sizeof(double);
      writeln('  ft=',ord(FieldType),' oft=',OFieldType,' ofs=',OFieldSize);
      
(*      case OFieldType of
        OCI_TYPECODE_NUMBER   : begin
                                //if OCIAttrGet(Param,OCI_DTYPE_PARAM,@Oprecision,nil,OCI_ATTR_PRECISION,FOciError) = OCI_ERROR then
                                 // HandleError;
                                //if OCIAttrGet(Param,OCI_DTYPE_PARAM,@Oscale,nil,OCI_ATTR_SCALE,FOciError) = OCI_ERROR then
                                  //HandleError;

                                if Oscale = 0 then
                                  begin
                                  FieldType := ftInteger;
                                  OFieldType := SQLT_INT;
                                  OFieldSize:= sizeof(integer);
                                  end
                                else if (oscale = -127) {and (OPrecision=0)} then
                                  begin
                                  FieldType := ftFloat;
                                  OFieldType := SQLT_FLT;
                                  OFieldSize:=sizeof(double);
                                  end
                                else if (oscale <=4) and (OPrecision<=12) then
                                  begin
                                  FieldType := ftBCD;
                                  FieldSize := sizeof(Currency);
                                  OFieldType := SQLT_VNU;
                                  OFieldSize:= 22;
                                  end
                                else FieldType := ftUnknown;
                                end;
        OCI_TYPECODE_CHAR,
        OCI_TYPECODE_VARCHAR,
        OCI_TYPECODE_VARCHAR2 : 
	  begin 
	    FieldType := ftString;
	    FieldSize := OFieldSize;
	    inc(OFieldsize);
	    OFieldType:=SQLT_STR
	  end;
        OCI_TYPECODE_DATE     : FieldType := ftDate;
        OCI_TYPECODE_TIMESTAMP,
        OCI_TYPECODE_TIMESTAMP_LTZ,
        OCI_TYPECODE_TIMESTAMP_TZ  : begin
                                     FieldType := ftDateTime;
                                     OFieldType := SQLT_ODT;
                                     end;*)
{      else
        FieldType := ftUnknown;
      end;}

      FieldBuffers[tel-1].buffer := getmem(OFieldSize);

      //FOciDefine := nil;
      //if OciDefineByPos(FOciStmt,FOciDefine,FOciError,tel,fieldbuffers[tel-1].buffer,OFieldSize,OFieldType,@(fieldbuffers[tel-1].ind),nil,nil,OCI_DEFAULT) = OCI_ERROR then
    //    HandleError;

      //if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldName,@OFNameLength,OCI_ATTR_NAME,FOciError) <> OCI_SUCCESS then
        //HandleError;

     //toto bude asi nieco dolezite!

    //  setlength(Fieldname,OFNameLength);
      //move(OFieldName^,Fieldname[1],OFNameLength);
      if tel=1 then
	  FieldName := 'i_tsta';
      if tel=2 then
	  FieldName := 'v_tsta';

      TFieldDef.Create(FieldDefs, FieldName, FieldType, FieldSize, False, tel);
      end;
  end;
end;

var asdf : integer = 0;

function TCsvDbConnection.Fetch(cursor: TSQLCursor): boolean;
begin
  writeln(ClassName+'.Fetch ...');
  inc(asdf);
  result := asdf < 3;
(*  case OCIStmtFetch2((cursor as TOracleCursor).FOciStmt,FOciError,1,OCI_FETCH_NEXT,1,OCI_DEFAULT) of
    OCI_ERROR   : begin
                  Result := False;
                  HandleError;
                  end;
    OCI_NO_DATA : Result := False;
    OCI_SUCCESS : Result := True;
    OCI_SUCCESS_WITH_INFO : Begin
                            Result := True;
                            HandleError;
                            end;
  end; {case}*)
end;

function TCsvDbConnection.LoadField(cursor: TSQLCursor; FieldDef: TFieldDef; buffer: pointer; out CreateBlob : boolean): boolean;

var dt        : TDateTime;
    b         : pbyte;
    size,i    :  byte;
    exp       : shortint;
    cur       : Currency;
    d : double;
//    q : integer;
//    odt       : POCIdateTime;

begin
  d := 3.14;
  writeln(ClassName+'.LoadField (fd.dt=',ord(FieldDef.DataType),' fd.fn=',FieldDef.FieldNo,')');
  CreateBlob := False;
  with cursor as TCsvDbCursor do 
    if fieldbuffers[FieldDef.FieldNo-1].ind = -1 then
      Result := False
  else
    begin
    result := True;
    case FieldDef.DataType of
      ftString          : move(fieldbuffers[FieldDef.FieldNo-1].buffer^,buffer^,FieldDef.Size);
      ftBCD             :  begin
                           b := fieldbuffers[FieldDef.FieldNo-1].buffer;
                           size := b[0];
                           cur := 0;
                           if (b[1] and $80)=$80 then // then the number is positive
                             begin
                             exp := (b[1] and $7f)-65;
                             for i := 2 to size do
                               cur := cur + (b[i]-1) * intpower(100,-(i-2)+exp);
                             end
                           else
                             begin
                             exp := (not(b[1]) and $7f)-65;
                             for i := 2 to size-1 do
                               cur := cur + (101-b[i]) * intpower(100,-(i-2)+exp);
                             cur := -cur;
                             end;
                           move(cur,buffer^,FieldDef.Size);
                           end;
      ftFloat           : 
        begin
	  writeln('  zapisujem float ...');
                d := 7.65;
                move(d,buffer^,sizeof(d));
//	  move(fieldbuffers[FieldDef.FieldNo-1].buffer^,buffer^,sizeof(double));

	end;
      ftInteger         : 
        begin
	  writeln('  zapisujem integer ...');
          move(fieldbuffers[FieldDef.FieldNo-1].buffer^,buffer^,sizeof(integer));
	end;
       ftDate  : begin
                b := fieldbuffers[FieldDef.FieldNo-1].buffer;
                dt := EncodeDate((b[0]-100)*100+(b[1]-100),b[2],b[3]);
                move(dt,buffer^,sizeof(dt));
                end;
{      ftDateTime : begin
                   odt := fieldbuffers[FieldDef.FieldNo-1].buffer;
                   dt := ComposeDateTime(EncodeDate(odt^.year,odt^.month,odt^.day), EncodeTime(odt^.hour,odt^.min,odt^.sec,0));
                   move(dt,buffer^,sizeof(dt));
                   end;}
    else
      Result := False;

    end;
    end;
end;

{function TCsvDbConnection.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:=inherited CreateBlobStream(Field, Mode);
end;}

procedure TCsvDbConnection.FreeFldBuffers(cursor: TSQLCursor);
begin
  writeln(ClassName+'.FreeFldBuffers');
  inherited FreeFldBuffers(cursor);
end;

constructor TCsvDbConnection.Create(AOwner: TComponent);
begin
  writeln(ClassName+'.Create');
  inherited Create(AOwner);
  FUserMem := nil;
end;

{ TCsvDbConnectionDef }

class function TCsvDbConnectionDef.TypeName: String;
begin
  Result:='CsvDb';
end;

class function TCsvDbConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TCsvDbConnection;
end;

class function TCsvDbConnectionDef.Description: String;
begin
  Result:='Connect to an CsvDb server';
end;

initialization
  randomize;
  RegisterConnection(TCsvDbConnectionDef);
finalization
  RegisterConnection(TCsvDbConnectionDef);
end.

