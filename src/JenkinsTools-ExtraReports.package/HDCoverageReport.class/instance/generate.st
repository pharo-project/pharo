generating
generate
	| coverage |
	covered := (wrappers select: [ :each | each hasRun ])
		collect: [ :each | each reference ].
	coverage := StandardFileStream 
		forceNewFileNamed: suite name , '-Coverage.xml'.
	[ self generateOn: coverage ]
		ensure: [ coverage close ]