private list model
keyPressedOnList: anEvent shifted: aBoolean
		| aCharacter |

	aCharacter := anEvent keyCharacter.

	(aCharacter == self model class nextFocusKey) ifTrue: [ ^ self model giveFocusTo: self model categoryWidget ].
	(aCharacter == self model class previousFocusKey) ifTrue: [ ^ self model giveFocusToPackage ].