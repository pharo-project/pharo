tests
testAttachKeymapAndExecuteExecutes
	| executed morphToTest |
	executed := false.
	
	KMFactory keymapContainer
		initializeKeymap: #test
		executingOn: $a asKeyCombination, $a asKeyCombination, $a asKeyCombination
		doing: [ :receiver | executed := true ]
		inCategory: #Testing
		platform: #all.

"	KMFactory keymapContainer attachCategoryName: #Testing to: Morph."
	
	morphToTest := Morph new.
	morphToTest attachKeymapCategory: #Testing.
	
	{self eventKey: $a. self eventKey: $a. self eventKey: $a}
		do: [:e | morphToTest dispatchKeystrokeForEvent: e].
		
	self assert: executed.