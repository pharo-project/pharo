debugger access
selector
	"Answer the selector of the method that created the receiver."

	^self method selector ifNil: [self method defaultSelector].