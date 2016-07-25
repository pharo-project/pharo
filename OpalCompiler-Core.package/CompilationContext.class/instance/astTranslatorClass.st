accessing
astTranslatorClass
	^ astTranslatorClass ifNil: [ astTranslatorClass := OCASTTranslator ]