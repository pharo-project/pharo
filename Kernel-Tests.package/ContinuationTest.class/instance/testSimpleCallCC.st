tests
testSimpleCallCC
	| x continuation |
	x := self callcc: [ :cc | continuation := cc. false ].
	x ifFalse: [ continuation value: true ].
	self assert: x