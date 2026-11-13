Extension { #name : 'MouseButtonEvent' }

{ #category : '*Calypso-Commands' }
MouseButtonEvent >> fixStateForClickAndModifierIssue [

	self isMouseUp ifFalse: [ ^ self ].
	self yellowButtonChanged ifFalse: [ ^ self ].
	self anyModifierKeyPressed ifFalse: [ ^ self ].

	self whichButton: MouseEvent redButton
]
