keyboard managing
setSelectionIndexFromKeyboard: index multiSelection: multiSelect event: anEvent
	"Called internally to select the index-th item."
	| targetMorph |
	index ifNil: [^ self].
	index > self allNodeMorphs size ifTrue: [^self].
	targetMorph := index = 0 ifTrue: [nil] ifFalse: [self allNodeMorphs at: index].
	lastClickedMorph ifNotNil: [lastClickedMorph highlightForMouseDown: false].
	self isCheckList
		ifTrue: [
			(multiSelect and: [anEvent shiftPressed])
				ifTrue: [self autoMultiSelect: targetMorph]]
		ifFalse: [
			(multiSelect and: [anEvent shiftPressed])
				ifTrue: [self autoMultiSelect: targetMorph]
				ifFalse: [self setSelectedMorph: targetMorph]].
	self lastClickedMorph: (firstClickedMorph := targetMorph).
	anEvent shiftPressed
		ifTrue: [lastClickedMorph highlightForMouseDown: true].

	self selectionChanged.
	self scrollToShow: targetMorph
	
