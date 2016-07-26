private-extensions-classfactoryfortestcase
newName
	<flExtensionOf: #ClassFactoryForTestCase>
	| postFix |
	postFix := (self createdClasses size + 1) printString.
	^(#ClassForTestToBeDeleted, postFix) asSymbol