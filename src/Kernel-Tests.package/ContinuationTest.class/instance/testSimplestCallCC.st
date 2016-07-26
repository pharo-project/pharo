tests
testSimplestCallCC
	| x |
	x := self callcc: [ :cc | cc value: true ].
	self assert: x