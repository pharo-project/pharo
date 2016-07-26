events
toggleExpandedState: aMorph event: event
	| oldState |
	"self setSelectedMorph: aMorph."
	event yellowButtonPressed ifTrue: [
		oldState := aMorph isExpanded.
		scroller submorphs copy do: [ :each |
			(each canExpand and: [each isExpanded = oldState]) ifTrue: [
				each toggleExpandedState.
			].
		].
	] ifFalse: [
		aMorph toggleExpandedState.
	].
	self adjustSubmorphPositions.
	