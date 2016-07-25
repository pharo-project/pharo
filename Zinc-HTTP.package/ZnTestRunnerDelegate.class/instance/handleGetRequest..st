request handling
handleGetRequest: request
	| testCaseClass selector result |
	testCaseClass := Smalltalk globals
		at: request uri pathSegments second asSymbol
		ifAbsent: [ ^ self notFoundResponse: 'No such class, ', request uri pathSegments second ].
	(testCaseClass inheritsFrom: TestCase)
		ifFalse: [ ^ self notFoundResponse: testCaseClass printString, ' is not a TestCase subclass' ]. 
	selector := request uri pathSegments size = 3
		ifTrue: [ request uri pathSegments third asSymbol ]
		ifFalse: [ nil ].
	(selector isNil or: [ testCaseClass canUnderstand: selector ])
		ifFalse: [ ^ self notFoundResponse: selector, ' is not a method of ', testCaseClass printString ].
	result := selector 
		ifNil: [ testCaseClass suite run ] 
		ifNotNil: [ testCaseClass run: selector ].
	^ result runCount = result passedCount
		ifTrue: [ ZnResponse ok: (ZnEntity textCRLF: result printString) ]
		ifFalse: [ ZnResponse serverError: result printString ]