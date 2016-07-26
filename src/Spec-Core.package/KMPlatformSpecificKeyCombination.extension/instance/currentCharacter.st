*Spec-Core
currentCharacter

	^ (self platform = #all or: [ self platform = OSPlatform current platformFamily ])
		ifTrue: [ self shortcut currentCharacter ]
		ifFalse: [ nil ]