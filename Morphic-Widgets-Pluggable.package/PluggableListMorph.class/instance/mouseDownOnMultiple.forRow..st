multi-selection
mouseDownOnMultiple: event forRow: row

	| anInteger oldIndex oldVal valueKeeper |
	
	"Set meaning for subsequent dragging of selection"
	canMove ifFalse: [ ^ self ].
	canMove := false.
	model okToChange ifFalse: [ 
		canMove := true.
		^ self ].
	canMove := true.
	
	dragOnOrOff := (self listSelectionAt: row) not.
	valueKeeper := dragOnOrOff.
	" I store the value because #mouseUpOnMultiple: can reset dragOnOrOff before the end of this method (in case of halt by example)"
	(event shiftPressed not and: [event yellowButtonPressed not and: [ self autoDeselect ]])
		ifTrue: [ self resetListSelection ].
	oldIndex := self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [oldVal := self listSelectionAt: oldIndex].

	"Set or clear new primary selection (listIndex)"
	anInteger := valueKeeper
					ifTrue: [ row ]
					ifFalse: [ 0 ].
	
	setIndexSelector ifNotNil:
		[ model perform: setIndexSelector with: anInteger ].
	
	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
	
	event shiftPressed
		ifTrue: [((oldIndex max: 1) min: row) to: (oldIndex max: row) do: [:i |
					self listSelectionAt: i put: valueKeeper].
				self changed]
		ifFalse: [self listSelectionAt: row put: valueKeeper].