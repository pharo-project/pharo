test situation
testExtensionSelectors
	"self debug: #testDefinedSelectors"
	
	self assert: a1 extensionSelectors isEmpty.

	self assert: a2 extensionSelectors size = 2.
	self assert: (a2 extensionSelectors includes: #methodDefinedInP1).
	self assert: (a2 extensionSelectors includes: #methodDefinedInP3).