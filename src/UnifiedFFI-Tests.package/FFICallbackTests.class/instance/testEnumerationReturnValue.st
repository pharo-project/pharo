tests
testEnumerationReturnValue
	| cb returnValue |
	"Issue 19145 - change semantics so that enumerations can be returned
	from callback without sending #value to it."
	cb := FFICallback
				signature:  #( FFITestEnumeration ())
				block: [ CCC ].
	"Dummy value of nil is possible since signature has no arguments"
	returnValue := cb valueWithContext: self sp: nil. 
	self assert: returnValue equals: CCC value.
	
	
	"Confirm backward compatibility okay"
	cb := FFICallback
				signature:  #( FFITestEnumeration ())
				block: [ CCC value ].
	"Dummy value of nil is possible since signature has no arguments"
	returnValue := cb valueWithContext: self sp: nil. 
	self assert: returnValue equals: CCC value.