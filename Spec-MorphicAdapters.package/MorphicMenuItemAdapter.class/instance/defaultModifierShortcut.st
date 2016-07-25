private
defaultModifierShortcut
	
	^ self shortcut first isUppercase
		ifTrue: [ OSPlatform current defaultModifier + KMModifier shift + self shortcut first ]
		ifFalse: [ OSPlatform current defaultModifier + self shortcut first ]