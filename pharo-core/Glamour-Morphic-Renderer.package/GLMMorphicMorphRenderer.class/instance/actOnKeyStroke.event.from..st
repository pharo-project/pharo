rendering
actOnKeyStroke: aPresentation event: anEvent from: aMorph 
	| action |

	anEvent commandKeyPressed ifFalse: [ ^self ].
	action := (self allKeyboardActionsFor: aPresentation) 
		detect: [ :each | each shortcut = anEvent keyCharacter  ] 
		ifNone: [ nil ].
		
	action 
		ifNotNil: [ action actOn: aPresentation].