Extension { #name : #TestCase }

{ #category : #'*SUnit-Support-UITesting' }
TestCase >> simulateKeyStroke: aCharacterOrShortcut inWorld: aWorld [

	aWorld activeHand simulateKeyStroke: aCharacterOrShortcut
]

{ #category : #'*SUnit-Support-UITesting' }
TestCase >> simulateKeyStrokes: aString inWorld: aWorld [

	aWorld activeHand simulateKeyStrokes: aString
]
