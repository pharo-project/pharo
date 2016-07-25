protocol
keyPressedOnCategory: anEvent
	| aCharacter |

	aCharacter := anEvent keyCharacter.

	(aCharacter == self model class nextFocusKey) ifTrue: [ ^ self model giveFocusTo: self model methodWidget ].
	(aCharacter == self model class previousFocusKey) ifTrue: [ ^ self model giveFocusTo: self model classWidget ].
	