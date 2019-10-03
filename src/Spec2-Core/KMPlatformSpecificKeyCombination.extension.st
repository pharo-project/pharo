Extension { #name : #KMPlatformSpecificKeyCombination }

{ #category : #'*Spec2-Core' }
KMPlatformSpecificKeyCombination >> spCurrentCharacter [

	^ (self platform = #all or: [ self platform = OSPlatform current platformFamily ])
		ifTrue: [ self shortcut spCurrentCharacter ]
		ifFalse: [ nil ]
]

{ #category : #'*Spec2-Core' }
KMPlatformSpecificKeyCombination >> spModifier [

	^ (self platform = #all or: [ self platform = OSPlatform current platformFamily ])
		ifTrue: [ self shortcut spModifier ]
		ifFalse: [ nil ]
]
