Extension { #name : #TestCase }

{ #category : #'*SUnit-UI-Tests' }
TestCase >> simulateKeyStroke: aCharacterOrShortcut [

	ActiveHand simulateKeyStroke: aCharacterOrShortcut.
]

{ #category : #'*SUnit-UI-Tests' }
TestCase >> simulateKeyStrokes: aString [ 

	ActiveHand simulateKeyStrokes: aString.
]
