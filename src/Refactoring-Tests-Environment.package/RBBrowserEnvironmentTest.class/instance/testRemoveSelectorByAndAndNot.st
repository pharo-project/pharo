testing
testRemoveSelectorByAndAndNot
	| aBrowserEnvironment objectPrintString |
	aBrowserEnvironment := RBBrowserEnvironment new.
	objectPrintString := RBSelectorEnvironment 
				onEnvironment: aBrowserEnvironment.
	objectPrintString addClass: Object selector: #printString.
	self assert: aBrowserEnvironment numberSelectors - 1 
				= (aBrowserEnvironment & objectPrintString not) numberSelectors.
	self universalTestFor: aBrowserEnvironment & objectPrintString not