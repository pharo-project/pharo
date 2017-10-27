generating
generate
	| coverage |
	covered := (wrappers select: [ :each | each hasRun ])
		collect: [ :each | each reference ].
	coverage := File openForWriteFileNamed: suite name , '-Coverage.xml'.
	[ self generateOn: coverage ]
		ensure: [ coverage close ]