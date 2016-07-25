testing-environments
testAndEnvironmentWithStoreOn
	| originalEnvironment objectPrintStringEnvironment andEnvironment |
	originalEnvironment := universalEnvironment referencesTo: #storeOn:.
	objectPrintStringEnvironment := universalEnvironment forClass: Object
				selectors: #(#storeString).
	andEnvironment := objectPrintStringEnvironment & originalEnvironment.
	self universalTestFor: andEnvironment.
	self assert: andEnvironment numberSelectors = 1.
	self assert: andEnvironment classNames asArray = #(#Object).
	self assert: (andEnvironment protocolsFor: Object) size = 1.
	self assert: (objectPrintStringEnvironment & objectPrintStringEnvironment) packages = objectPrintStringEnvironment packages.
	andEnvironment := originalEnvironment 
				& (universalEnvironment referencesTo: #printString).
	self assert: andEnvironment numberSelectors 
				= (originalEnvironment referencesTo: #printString) numberSelectors.
	self assert: andEnvironment classNames asSortedCollection 
				= (originalEnvironment referencesTo: #printString) classNames 
						asSortedCollection