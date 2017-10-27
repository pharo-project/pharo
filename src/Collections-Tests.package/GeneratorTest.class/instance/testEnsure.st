tests
testEnsure
	| generator |
	generator := Generator on: [ :g |
		[ g yield: 1; yield: 2 ]
			ensure: [ g yield: 3 ] ].
	self assert: generator upToEnd asArray = #( 1 2 3 )