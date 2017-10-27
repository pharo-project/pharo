tests
testSimple
	| generator |
	generator := Generator on: [ :g | g yield: 1; yield: 2 ].
	self assert: generator upToEnd asArray = #( 1 2 )