truncation and round off
rounded
	"Answer the integer nearest the receiver.
	Implementation note: super would not handle tricky inexact arithmetic"
	
	"self assert: 5000000000000001.0 rounded = 5000000000000001"

	self fractionPart abs < 0.5
		ifTrue: [^self truncated]
		ifFalse: [^self truncated + self sign rounded]