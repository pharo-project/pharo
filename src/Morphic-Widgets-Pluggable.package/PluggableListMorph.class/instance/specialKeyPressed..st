events
specialKeyPressed: anEvent
	"A special key with the given ascii-value was pressed; dispatch it"
	| keyString max nextSelection oldSelection howManyItemsShowing |
	
	keyString := anEvent keyString.
	keyString = '<escape>'
		ifTrue: [" escape key"
			^ ActiveEvent shiftPressed
				ifTrue:
					[ActiveWorld invokeWorldMenuFromEscapeKey]
				ifFalse:[ 
					(self yellowButtonActivity: false) 
						ifTrue: [ ^ self ]]].

	keyString = '<cr>'
		ifTrue: [ 
			"enter pressed" 
			self selectSearchedElement ].

	max := self maximumSelection.
	max > 0 ifFalse: [^ self].
	nextSelection := oldSelection := self getCurrentSelectionIndex.
	
	keyString = '<down>'
		ifTrue: [" down arrow"
			self resetListSelectionSilently.
			nextSelection := oldSelection + 1.
			nextSelection > max ifTrue: [nextSelection := max]].
	
	keyString = '<up>'
		ifTrue: [ " up arrow"
			self resetListSelectionSilently.
			nextSelection := oldSelection - 1.
			nextSelection < 1 ifTrue: [nextSelection := 1]].
	keyString = '<home>'
		ifTrue: [" home"
			self resetListSelectionSilently.
			nextSelection := 1].
		
	keyString = '<end>' 
		ifTrue: [" end"
			self resetListSelectionSilently.
			nextSelection := max].
	
	howManyItemsShowing := self numSelectionsInView.
	
	keyString = '<pageUp>'
		ifTrue: 	[" page up"
			self resetListSelectionSilently.
			nextSelection := 1 max: oldSelection - howManyItemsShowing].
	keyString = '<pageDown>'
		ifTrue: [" page down"
			self resetListSelectionSilently.
			nextSelection := oldSelection + howManyItemsShowing min: max].

	(self enabled and: [model okToChange]) ifFalse: [^ self].
	
	"No change if model is locked"
	
	oldSelection = nextSelection ifTrue: [^ self].
	^ self changeModelSelection: nextSelection