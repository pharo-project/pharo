Class {
	#name : #KeymapBuilderTest,
	#superclass : #AbstractKeymappingTest,
	#category : #Keymapping-Tests
}

{ #category : #tests }
KeymapBuilderTest >> testAddKeymapCreatesCategory [
	self assert: KMRepository default categories isEmpty.
	
	KMRepository default
		initializeKeymap: #test
		executingOn: $r ctrl, $r asKeyCombination, $r asKeyCombination
		doing: [ :receiver | "nothing" ]
		inCategory: #Testing
		platform: #all.
	
	self assert: (KMRepository default includesCategoryNamed: #Testing).
	self assert: KMRepository default categories size = 1.
]

{ #category : #tests }
KeymapBuilderTest >> testAddKeymapCreatesShortcut [
	KMRepository default
		initializeKeymap: #test
		executingOn: $r ctrl, $r asKeyCombination, $r asKeyCombination
		doing: [ :receiver | "nothing" ]
		inCategory: #Testing
		platform: #all.
	
	self assert: (KMRepository default categoryForName: #Testing ) allEntries size = 1.
]

{ #category : #tests }
KeymapBuilderTest >> testAttachKeymapAndExecuteExecutes [
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
]
