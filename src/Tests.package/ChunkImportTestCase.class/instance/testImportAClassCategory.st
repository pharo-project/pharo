importing-code
testImportAClassCategory
	| class classOrganizationString chunk |
	classOrganizationString := '(#testing testImportAMethod testImportAClass)
(#auxiliar importAClass)'.
	chunk := '!{1} reorganize!'.
	SystemAnnouncer uniqueInstance suspendAllWhile: [ [
		class := self importAClass.
		
		"we write the methods we will categorize"
		class compileSilently: 'testImportAClass'.
		class compileSilently: 'testImportAMethod'.
		class compileSilently: 'importAClass'.
		
		CodeImporter evaluateString: (chunk format: { class name asString }), classOrganizationString.
		"we do not care about the order of the output just that all the elements are present"
		self 
			assertCollection: (classOrganizationString trimBoth findTokens: String cr, ' ()') sorted
			equals: (class organization stringForFileOut findTokens: String cr, ' ()') sorted.
	] ensure: [ class ifNotNil: [ class removeFromSystem ] ] ]