testing
isModifierPressed: anEvent

	self hintModifier ifNil: [ ^ false ].
	self hintModifier = #shift ifTrue: [ ^ anEvent shiftPressed ].
	self hintModifier = #option ifTrue: [ ^ anEvent optionKeyPressed ].
	self hintModifier = #alt ifTrue: [ ^ anEvent altKeyPressed ].
	self hintModifier = #command ifTrue: [ ^ anEvent commandKeyPressed ].
	self hintModifier = #control ifTrue: [ ^ anEvent controlKeyPressed ].
	^ false