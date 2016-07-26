protocol
keyPressedOnElement: anEvent
	| aCharacter |
	
	aCharacter := anEvent keyCharacter.

	aCharacter == self model class nextFocusKey ifTrue: [ ^ self model giveFocusToSourceCodeArea ].	
	aCharacter == self model class previousFocusKey ifTrue: [ ^ self model giveFocusTo: self model categoryWidget ].
