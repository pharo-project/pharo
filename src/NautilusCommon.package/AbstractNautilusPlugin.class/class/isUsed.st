testing
isUsed
	"all my sublasses are used"
	^self name = 'AbstractNautilusPlugin'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]