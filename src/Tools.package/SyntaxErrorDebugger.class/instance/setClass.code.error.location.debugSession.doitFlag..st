initialization
setClass: aClass code: aString error: errorMessage location: anErrorPosition debugSession: aDebugSession doitFlag: flag

	class := aClass.
	error := errorMessage.
	location := anErrorPosition.	
	debugSession := aDebugSession.
	selector := aClass compiler parseSelector: aString.
	contents := self checkForUnprintableCharacters: aString.
	self highlightError.
		
	category ifNil: [ category := aClass organization categoryOfElement: selector ].
	category ifNil: [ category := Protocol unclassified ].
	doitFlag := flag