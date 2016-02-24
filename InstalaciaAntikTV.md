# Ako root #

```
# fpc compiler, fpc source

wget 'ftp://ftp.freepascal.org/pub/fpc/dist/2.4.0/i386-linux/fpc-2.4.0.i386-linux.tar'
wget 'ftp://ftp.freepascal.org/pub/fpc/dist/2.4.0/source/fpc-2.4.0.source.tar.gz'

# nainstalujeme FPC (treba zvolit /usr/local, stale stlacat enter)

tar xpvf fpc-2.4.0.i386-linux.tar
su
./install.sh

# rozbalime zdrojaky FPC

tar zxpf fpc-2.4.0.source.tar.gz -C /usr/local/lib/fpc/2.4.0
mv /usr/local/lib/fpc/2.4.0/fpc-2.4.0 /usr/local/lib/fpc/2.4.0/source
```

# Ako obyčajný používateľ #

```
# stiahneme a rozbalime zdrojaky lazarusa
wget 'http://downloads.sourceforge.net/project/lazarus/Lazarus%20Zip%20_%20GZip/Lazarus%200.9.28.2/lazarus-0.9.28.2-src.tar.bz2?use_mirror=garr'
tar jxpf lazarus-0.9.28.2-src.tar.bz2

# kompilacia lazarusa

sudo apt-get install build-essential libx11-dev libgdk-pixbuf-dev libgtk2.0-dev subversion
(toto mozno bude treba: sudo ln -s /usr/lib/libgdk_pixbuf-2.0.so.0 libgdk_pixbuf-2.0.so)
cd lazarus
make clean
make

# stiahnutie a kompilacia antiktv

mkdir ~/antiktv
cd ~/antiktv
svn checkout http://antiktv.googlecode.com/svn/trunk/

Spustite startlazarus, ignorujte varningy, v menu Prostredie -> Volby prostredia nastavte:

Adresar lazarus: /home/VASE_MENO/lazarus/
Cesta prekladaca: /usr/local/bin/fpc
Adresar zdrojovych kodov FPC: /usr/local/lib/fpc/2.4.0/source
OK

Subor->Otvorit->/home/VASE_MENO/antiktv/trunk/antik.lpi
Spustit->Vybudovat vsetko

Po uspesnej kompilacii (pripadne chyby v terminale alebo v spec okne hlasok) je binarka v /home/VASE_MENO/antiktv/trunk/antiktv
Ak funguje mozete ju stripnut (strip antiktv) aby mala mensiu velkost
Pozn: Musite mat nainstalovany wget a vlc
```