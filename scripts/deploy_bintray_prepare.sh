echo "`cat .bintray.json | scripts/deploy_bintray_prepare_metadata.smudge`" > .bintray.json
sed -i.bak 's/$Arch\$/'"$BOOTSTRAP_ARCH"'/' .bintray.json
