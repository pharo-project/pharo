testing
isUsed
	"all my subclasses are used by me"
	^self name = 'TestCase'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]