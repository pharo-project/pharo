tests
testIfTrueWithClosureAfterJump
	"A closure is a multibyte instruction, that should not be
	1) confused for an ending jump in ifTrue/ifFalse structure
	2) set as jumptarget, or have PC manually set to, none but its first byte."
	| myBooleanObject |
	myBooleanObject := MyBooleanObject new.
	myBooleanObject
		ifTrue: [ 4 + 3 ].
	[ :string | 
	self assert: 'a ' , string equals: 'a 5' ] value: '5'