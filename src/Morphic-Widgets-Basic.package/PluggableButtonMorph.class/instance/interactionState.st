accessing
interactionState
	"Return the state that the receiver is in, #pressed, #normal etc.."
	
	|state mo|
	state := self getModelState.
	mo := (self valueOfProperty: #mouseEntered) == true.
	
	^(self enabled ifNil: [true])
		ifTrue: [showSelectionFeedback
			ifTrue: [state
				ifTrue: [#selectedPressed]
				ifFalse: [#pressed]]
			ifFalse: [mo
				ifTrue: [state
					ifTrue: [#selectedMouseOver]
					ifFalse: [#mouseOver]]
				ifFalse: [state
					ifTrue: [#selected]
					ifFalse: [#normal]]]]
		ifFalse: [state
			ifTrue: [#selectedDisabled]
			ifFalse: [#disabled]]