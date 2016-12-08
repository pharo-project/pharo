patching
environmentWithSlotValidationDisabled: aRFiEnvironment
	"We need to disable the class name validation because it will end to call EncodedCharSet>>#charsetAt: but this class is not yet initialized and will throw errors"

	| edit |
	edit := aRFiEnvironment ficusPackages 
		editAtPath: FiPath root / #Slot / #contents / #'Extensions to String' / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'asSlotCollection
	"Parse as slots.
	Example: 
	  ''a b'' asSlotCollection --> {a => Slot. b => Slot}
	"
	^(self substrings: '' '') collect: [ :substring | substring asSlot ]	') ].
	^ (aRFiEnvironment ficusPackages resultOf: edit) asRFiEnvironment