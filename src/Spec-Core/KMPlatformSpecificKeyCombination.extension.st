Extension { #name : #KMPlatformSpecificKeyCombination }

{ #category : #'*Spec-Core' }
KMPlatformSpecificKeyCombination >> currentCharacter [

	^ (self platform = #all or: [ self platform = OSPlatform current platformFamily ])
		ifTrue: [ self shortcut currentCharacter ]
		ifFalse: [ nil ]
]
