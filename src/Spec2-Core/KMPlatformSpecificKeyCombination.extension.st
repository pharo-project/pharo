Extension { #name : #KMPlatformSpecificKeyCombination }

{ #category : #'*Spec2-Core' }
KMPlatformSpecificKeyCombination >> spCurrentCharacter [

	^ (self platform = #all or: [ self platform = OSPlatform current platformFamily ])
		ifTrue: [ self shortcut spCurrentCharacter ]
		ifFalse: [ nil ]
]
