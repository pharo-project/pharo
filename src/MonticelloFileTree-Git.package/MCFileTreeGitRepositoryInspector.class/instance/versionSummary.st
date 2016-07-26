accessing
versionSummary
	^ (Cursor wait showWhile: [ (versions detect: [ :e | e last name = selectedVersion ]) last ]) summary