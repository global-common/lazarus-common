#! bash -uvx
set -e
cwd=`pwd`
rm -rf openssl64.tmp
mkdir -p openssl64.tmp
cd openssl64.tmp
dark -x vc_redist.x64.tmp ../vc_redist.x64.exe
7z x -y vc_redist.x64.tmp/AttachedContainer/packages/vcRuntimeMinimum_amd64/cab1.cab
#7z x -y vc_redist.x64.tmp/AttachedContainer/packages/vcRuntimeAdditional_amd64/cab1.cab
for fname in *.dll_amd64; do
  mv $fname ${fname%.dll_amd64}.dll;
done
rm -rf vc_redist.x64.tmp
cp -rp C:/lazarus/libcrypto-*.dll .
cp -rp C:/lazarus/libssl-*.dll .
7z a -tzip ../openssl64.zip *
