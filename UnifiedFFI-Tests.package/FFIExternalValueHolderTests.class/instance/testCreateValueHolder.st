tests
testCreateValueHolder
	| valueHolderClass object |
	
	valueHolderClass := FFIExternalValueHolder ofType: 'long'.
	object := valueHolderClass new.
	
	self assert: object type class equals: FFIInt32.
	self assert: object value equals: 0.
	
	object value: 42.
	self assert: object value equals: 42.
