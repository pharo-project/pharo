events-processing
toggleExpandedState: aMorph event: event
	| oldState |
	event yellowButtonPressed 
		ifTrue: [
			oldState := aMorph isExpanded.
			self allNodeMorphs copy do: [ :each |
				(each canExpand and: [each isExpanded = oldState]) 
					ifTrue: [each toggleExpandedState]]] 
		ifFalse: [aMorph toggleExpandedState].
	self adjustSubmorphPositions.
	