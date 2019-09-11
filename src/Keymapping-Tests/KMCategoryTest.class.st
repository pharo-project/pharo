"
SUnit tests for key mapping categories
"
Class {
	#name : #KMCategoryTest,
	#superclass : #AbstractKeymappingTest,
	#category : #'Keymapping-Tests'
}

{ #category : #accessing }
KMCategoryTest >> categoryContainer [
	^ KMFactory keymapContainer
]

{ #category : #tests }
KMCategoryTest >> testAddKeymapToCategory [
	| categoryToAdd entry |
	categoryToAdd := KMCategory named: #TestCategory.
	entry := KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [ "nothing" ].

	self assert: (categoryToAdd allEntries) size equals: 0.
	categoryToAdd addKeymapEntry: entry.
	self assert: (categoryToAdd allEntries) size equals: 1
]

{ #category : #tests }
KMCategoryTest >> testCreateExistentCategoryFails [
	| categoryToAdd |
	categoryToAdd := KMCategory named: #TestCategory.
	
	self assert: self categoryContainer categories size equals: 0.
	self categoryContainer addCategory: categoryToAdd.
	self assert: self categoryContainer categories size equals: 1.
	self should: [self categoryContainer addCategory: categoryToAdd] raise: Error
]

{ #category : #tests }
KMCategoryTest >> testCreateUnexistentCategory [
	| categoryToAdd |
	categoryToAdd := KMCategory named: #TestCategory.
	
	self assert: self categoryContainer categories size equals: 0.
	self categoryContainer addCategory: categoryToAdd.
	self assert: self categoryContainer categories size equals: 1.
	self assert: (self categoryContainer includesCategory: categoryToAdd)
]
