constants
fmin
	"Answer minimum positive representable value."
	
	^self denormalized
		ifTrue: [self fminDenormalized]
		ifFalse: [self fminNormalized]