menu
outermostOwnerWithYellowButtonMenu
	"Answer me or my outermost owner that is willing to contribute menu items to a context menu.
	Don't include the world."

	| outermost |
	outermost := self outermostMorphThat: [ :ea |
		ea isWorldMorph not and: [ ea hasYellowButtonMenu ]].
	^outermost ifNil: [ self hasYellowButtonMenu ifTrue: [ self ] ifFalse: []] 