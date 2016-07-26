*GT-Inspector-Examples-UI
examplesReferencesTo: aLiteral
	^ GTExampleFinder allGTExamples select: [ :each | 
			(each hasSubject: aLiteral)
				or: [ (each method hasLiteralThorough: aLiteral)
					or: [ (each source theNonMetaClass name asString = aLiteral name asString) ] ] ]