testing - misc
testExampleEffectValues
	| cm |
	cm := OCOpalExamples compiler compile: (OCOpalExamples>>#exampleEffectValues) sourceCode.
	self assert: (cm literals includes: #getMe).
	self assert: (cm literals includes: Class binding).
	self assert: (cm literals includes: #( got that ? )).