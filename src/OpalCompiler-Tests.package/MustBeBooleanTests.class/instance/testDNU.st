tests
testDNU
	| code |
	code := [ (true ifFalse:[]) ifTrue:[] ].
	self should: code raise: MessageNotUnderstood .
	code on: MessageNotUnderstood do: [ :error | self assert: error receiver isNil. 
			self assert: error message selector equals: #ifTrue:]