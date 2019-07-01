"
A SycRefactoringPreview2Test is a test class for testing the behavior of SycRefactoringPreview2
"
Class {
	#name : #SycRefactoringPreview2Test,
	#superclass : #TestCase,
	#instVars : [
		'sycRefactoringPreview'
	],
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #tests }
SycRefactoringPreview2Test >> fillTableWith3MockItem [
	| mockTreeTableItem2 mockTreeTableItem3 mockTreeTableItem1 |
	mockTreeTableItem1 := MockTreeTableItem new.
	mockTreeTableItem2 := MockTreeTableItem new.
	mockTreeTableItem3 := MockTreeTableItem new.
	sycRefactoringPreview table
		items:
			{mockTreeTableItem1.
			mockTreeTableItem2.
			mockTreeTableItem3}
]

{ #category : #setup }
SycRefactoringPreview2Test >> initializeForInitializeTest [
	| scopeClass scopePackage |
	scopePackage := SycMockPackageScope new.
	scopeClass := SycMockClassScope new.
	sycRefactoringPreview := MockSycRefactoringPreview
		for: SycRefactoringPreviewMockCommand new
		scopes:
			{scopePackage.
			scopeClass}
]

{ #category : #initialization }
SycRefactoringPreview2Test >> initializeSycRefactoringUsingForScope [
	| scopeClass scopePackage |
	scopePackage := SycMockPackageScope new .
	scopeClass := SycMockClassScope new.
	sycRefactoringPreview := MockSycRefactoringPreview2
		for: SycRefactoringPreviewMockCommand new
		scopes:
			{scopePackage.
			scopeClass}
]

{ #category : #running }
SycRefactoringPreview2Test >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	sycRefactoringPreview := MockSycRefactoringPreview  new.
]

{ #category : #tests }
SycRefactoringPreview2Test >> testDoubleClickAnTableItemShouldChangeTheValueAssociateToKey [
	| mockTreeTableItem1 mockTreeTableItem2 mockTreeTableItem3 |
	mockTreeTableItem1 := MockTreeTableItem new.
	mockTreeTableItem2 := MockTreeTableItem new.
	mockTreeTableItem3 := MockTreeTableItem new.
	sycRefactoringPreview table
		items:
			{mockTreeTableItem1.
			mockTreeTableItem2.
			mockTreeTableItem3}.
	sycRefactoringPreview selectedRefactorings
		at: mockTreeTableItem3
		put: true.
	sycRefactoringPreview table selectIndex: 3.
	sycRefactoringPreview table doActivateAtIndex: 3.
	self
		deny: (sycRefactoringPreview selectedRefactorings at: mockTreeTableItem3)
]

{ #category : #tests }
SycRefactoringPreview2Test >> testDropListChangeShouldChangeTableItems [
	"this method can't be test because i can't isolate the command"

	| tableItemBeforeChangeDropList className |
	self initializeSycRefactoringUsingForScope.
	sycRefactoringPreview scopeDropList selectedIndex: 2.
	self
		assert: sycRefactoringPreview table items
		equals: {'banane' . 'Tournai Triple'} asOrderedCollection
]

{ #category : #'initialize-test' }
SycRefactoringPreview2Test >> testInitAllCheckBoxDictionarySetTrue [
	"this method can't be test because i can't isolate the command"
	self initializeForInitializeTest.
	self assert: sycRefactoringPreview selectedRefactorings size equals: 3.
	self
		assert:
			(sycRefactoringPreview selectedRefactorings
				allSatisfy: [ :key | key ])
			
]

{ #category : #tests }
SycRefactoringPreview2Test >> testPickedChanges [
	self fillTableWith3MockItem.
	sycRefactoringPreview selectectAllCheckBox.
	sycRefactoringPreview table selectIndex: 2.
	sycRefactoringPreview table doActivateAtIndex: 2.
	self
		deny:
			(sycRefactoringPreview selectedRefactorings
				includesKey: sycRefactoringPreview table selection selectedIndex)
]
