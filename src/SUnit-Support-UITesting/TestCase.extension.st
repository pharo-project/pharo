Extension { #name : #TestCase }

{ #category : #'*SUnit-Support-UITesting' }
TestCase >> simulateKeyStroke: aCharacterOrShortcut [

	ActiveHand simulateKeyStroke: aCharacterOrShortcut.
]

{ #category : #'*SUnit-Support-UITesting' }
TestCase >> simulateKeyStrokes: aString [ 

	ActiveHand simulateKeyStrokes: aString.
]
