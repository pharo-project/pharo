patching
environmentWithTraitNameValidationDisbaledFrom: aRFiEnvironment
	"We need to disable the class name validation because it will end to call EncodedCharSet>>#charsetAt: but this class is not yet initialized and will throw errors"

	| edit |
	edit := aRFiEnvironment ficusPackages 
		editAtPath: FiPath root / #Traits / #contents / #TClass / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 
			'isValidTraitName: aSymbol 
				^ true' ) ].
	^ (aRFiEnvironment ficusPackages resultOf: edit) asRFiEnvironment
