menu
addCustomMenuItems: aCustomMenu hand: aHandMorph
	| outer |
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addUpdating: #autoFitString target: self selector: #autoFitOnOff.
	aCustomMenu addUpdating: #wrapString target: self selector: #wrapOnOff.
	aCustomMenu add: 'text margins...' selector: #changeMargins:.
	aCustomMenu add: 'add predecessor' selector: #addPredecessor:.
	aCustomMenu add: 'add successor' selector: #addSuccessor:.
	aCustomMenu add: 'code pane menu...' selector: #yellowButtonActivity.
	aCustomMenu add: 'code pane shift menu...' selector: #shiftedYellowButtonActivity.

	outer := self owner.
	outer ifNotNil: [
	outer isLineMorph ifTrue:
		[container isNil
			ifFalse: [aCustomMenu add: 'reverse direction' selector: #reverseCurveDirection.
					aCustomMenu add: 'set baseline' selector: #setCurveBaseline:]]
		ifFalse:
		[self fillsOwner
			ifFalse: [aCustomMenu add: 'fill owner''s shape' selector: #fillingOnOff]
			ifTrue: [aCustomMenu add: 'rectangular bounds' selector: #fillingOnOff].
		self avoidsOcclusions
			ifFalse: [aCustomMenu add: 'avoid occlusions' selector: #occlusionsOnOff]
			ifTrue: [aCustomMenu add: 'ignore occlusions' selector: #occlusionsOnOff]]].
	