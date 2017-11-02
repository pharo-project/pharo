Extension { #name : #UndefinedObjectTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
UndefinedObjectTest >> testStoreOn [
	| string |
	string := String streamContents: [ :stream | nil storeOn: stream ].
	self assert: (self class compiler evaluate: string) isNil
]
