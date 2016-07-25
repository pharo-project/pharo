testing
isUsed
	"all my sublasses are used"
	^self name = 'RBLintRule'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]