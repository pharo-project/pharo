Extension { #name : 'UndefinedObjectTest' }

{ #category : '*Kernel-Tests-WithCompiler' }
UndefinedObjectTest >> testStoreOnRoundTrip [
	| string |
	string := String streamContents: [ :stream | nil storeOn: stream ].
	self assert: (OpalEvaluator new evaluate: string) isNil
]
