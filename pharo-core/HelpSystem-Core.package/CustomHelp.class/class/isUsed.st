testing
isUsed
	"all my sublasses are used"
	^self name = 'CustomHelp'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]