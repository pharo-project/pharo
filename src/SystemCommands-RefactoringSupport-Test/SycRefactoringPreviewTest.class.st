"
A SycRefactoringPreview2Test is a test class for testing the behavior of SycRefactoringPreview2
"
Class {
	#name : #SycRefactoringPreviewTest,
	#superclass : #TestCase,
	#instVars : [
		'sycRefactoringPreview'
	],
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #running }
SycRefactoringPreviewTest >> fillTableWith3MockItem [
	sycRefactoringPreview table
		items:
			{MockTreeTableItem new.
			MockTreeTableItem new.
			MockTreeTableItem new}
]

{ #category : #running }
SycRefactoringPreviewTest >> initializeForInitializeTest [
	| scopeClass scopePackage |
	scopePackage := SycMockScope content: {'welsh' . 'banane' . 'paix dieu'}.
	scopeClass := SycMockScope content: {'banane' . 'Tournai Triple'}.
	sycRefactoringPreview := MockSycRefactoringPreview for: SycRefactoringPreviewMockCommand new scopes: {scopePackage . scopeClass}
]

{ #category : #running }
SycRefactoringPreviewTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	sycRefactoringPreview := MockSycRefactoringPreview new
]

{ #category : #tests }
SycRefactoringPreviewTest >> testDoubleClickAnTableItemShouldChangeTheValueAssociateToKey [
	| mock2 |
	mock2 := MockTreeTableItem new title: 'mockTable2'.
	sycRefactoringPreview table
		items:
			{(MockTreeTableItem new title: 'mockTable1').
			mock2}.
	sycRefactoringPreview selectectAllCheckBox.
	sycRefactoringPreview selectedRefactorings at: mock2 put: true.
	sycRefactoringPreview table selectIndex: 2.
	sycRefactoringPreview table doActivateAtIndex: 2.
	self deny: (sycRefactoringPreview selectedRefactorings at: mock2)
]

{ #category : #tests }
SycRefactoringPreviewTest >> testDropListChangeShouldChangeTableItems [
	self initializeForInitializeTest.
	sycRefactoringPreview scopeDropList selectedIndex: 2.
	self assertCollection: sycRefactoringPreview table items hasSameElements: {'banane' . 'Tournai Triple'}
]

{ #category : #tests }
SycRefactoringPreviewTest >> testFirstItemIsSelected [
	self initializeForInitializeTest.
	self assert: sycRefactoringPreview table selection selectedIndex equals: 1
]

{ #category : #tests }
SycRefactoringPreviewTest >> testInitializationAllCheckBoxDictionarySetTrue [
	self initializeForInitializeTest.
	self assert: sycRefactoringPreview selectedRefactorings size equals: 3.
	self
		assert:
			(sycRefactoringPreview selectedRefactorings
				allSatisfy: [ :key | key ])
			
]

{ #category : #tests }
SycRefactoringPreviewTest >> testPickedChanges [
	self fillTableWith3MockItem.
	sycRefactoringPreview selectectAllCheckBox.
	sycRefactoringPreview table selectIndex: 2.
	sycRefactoringPreview table doActivateAtIndex: 2.
	self deny: (sycRefactoringPreview selectedRefactorings includesKey: sycRefactoringPreview table selection selectedIndex)
]
