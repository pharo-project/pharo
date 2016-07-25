commands
add: modelGetter withSpec: aSymbol wrapped: aBlock
	| wrapper |
	wrapper := SpecWrapperTableLayout wrap: modelGetter withSpec: aSymbol.
	aBlock value: wrapper.
	self add: wrapper
	