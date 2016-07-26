testing
testSelectMethods
	| environment |
	environment := RBBrowserEnvironment new selectMethods: [:each | false].
	self assert: environment numberSelectors = 0.
	self assert: environment numberClasses = 0.
	environment := RBBrowserEnvironment new selectMethods: [:each | true].
	self assert: environment numberSelectors 
				= RBBrowserEnvironment new numberSelectors.
	environment := RBBrowserEnvironment new 
				selectMethods: [:each | each refersToLiteral: #environment].
	self assert: environment numberSelectors 
				= (RBBrowserEnvironment new referencesTo: #environment) numberSelectors