accessing
versionInfo
	^ (Cursor wait showWhile: [ (versions detect: [ :e | e last name = selectedVersion ]) last ]) info