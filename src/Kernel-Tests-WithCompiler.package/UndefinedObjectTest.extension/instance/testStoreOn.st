*Kernel-Tests-WithCompiler
testStoreOn
	| string |
	string := String streamContents: [ :stream | nil storeOn: stream ].
	self assert: (self class compiler evaluate: string) isNil