copying
copyBitsTranslucent: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"Check for compressed source, destination or halftone forms"
	((sourceForm isForm) and: [sourceForm unhibernate])
		ifTrue: [^ self copyBitsTranslucent: factor].
	((destForm isForm) and: [destForm unhibernate])
		ifTrue: [^ self copyBitsTranslucent: factor].
	((halftoneForm isForm) and: [halftoneForm unhibernate])
		ifTrue: [^ self copyBitsTranslucent: factor].

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"