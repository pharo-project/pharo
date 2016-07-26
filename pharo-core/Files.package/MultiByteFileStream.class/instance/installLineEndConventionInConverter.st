private
installLineEndConventionInConverter

	converter ifNotNil: [
		converter installLineEndConvention: (
			(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
				ifTrue: [ LineEndStrings at: lineEndConvention ]
				ifFalse: [ nil ]) ]