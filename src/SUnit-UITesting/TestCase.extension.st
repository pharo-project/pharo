Extension { #name : #TestCase }

{ #category : #'*SUnit-UITesting' }
TestCase >> simulateKeyStroke: aCharacterOrShortcut [

	ActiveHand simulateKeyStroke: aCharacterOrShortcut.
]

{ #category : #'*SUnit-UITesting' }
TestCase >> simulateKeyStrokes: aString [ 

	ActiveHand simulateKeyStrokes: aString.
]
