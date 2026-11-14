"
Tests for ClySelectionContextDeclaration, on the context declarations of the full and query browsers.
"
Class {
	#name : 'ClySelectionContextDeclarationTest',
	#superclass : 'TestCase',
	#category : 'Calypso-SystemTools-FullBrowser-Tests',
	#package : 'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : 'private' }
ClySelectionContextDeclarationTest >> declarationOf: aContextClass itemType: anItemType [

	^ ClySelectionContextDeclaration all detect: [ :each | each contextClass == aContextClass and: [ each itemType == anItemType ] ]
]

{ #category : 'tests' }
ClySelectionContextDeclarationTest >> testDeclarationAppliesToKindsOfItsBrowser [

	| declaration |
	declaration := self declarationOf: ClyFullBrowserMethodContext itemType: ClyMethod.
	self assert: (declaration isForBrowser: ClyFullBrowserMorph basicNew).
	self deny: (declaration isForBrowser: ClyQueryBrowserMorph basicNew)
]

{ #category : 'tests' }
ClySelectionContextDeclarationTest >> testDeclarationWithoutItemTypeAppliesToAnySelection [

	self assert: ((ClySelectionContextDeclaration browser: ClyQueryBrowserMorph) shouldBeAppliedFor: nil)
]

{ #category : 'tests' }
ClySelectionContextDeclarationTest >> testDeclarationsKnowTheirMethod [

	self
		assert: (self declarationOf: ClyFullBrowserMethodContext itemType: ClyMethod) declaringMethod
		identicalTo: ClyFullBrowserMethodContext class >> #selectionContextDeclaration
]

{ #category : 'tests' }
ClySelectionContextDeclarationTest >> testHigherPriorityIsTriedFirst [

	| all |
	all := ClySelectionContextDeclaration all.
	self assert: (all indexOf: (self declarationOf: ClyFullBrowserVariableContext itemType: ClyVariableMethodGroup)) < (all indexOf: (self declarationOf: ClyFullBrowserMethodGroupContext itemType: ClyMethodGroup))
]

{ #category : 'tests' }
ClySelectionContextDeclarationTest >> testUnknownBrowserGetsUnknownContext [

	self assert: (ClySelectionContextDeclaration selectContextOf: nil for: Object new) class identicalTo: ClyUnknownBrowserContext
]
