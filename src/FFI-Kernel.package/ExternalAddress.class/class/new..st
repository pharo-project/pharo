instance creation
new: n
	"Only create ExternalAddresses of the right size."
	^n = wordSize
		ifTrue: [super new: n]
		ifFalse: [self shouldNotImplement]