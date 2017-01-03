echo "`cat .bintray.json | scripts/deploy_bintray_prepare_metadata.smudge`" > .bintray.json
sed -i.bak 's/$Rev: \([0-9][0-9]*\) \$/\1/' .bintray.json
sed -i.bak 's/$Date: \(.*\) \$/\1/' .bintray.json
rm -f .bintray.json.bak
