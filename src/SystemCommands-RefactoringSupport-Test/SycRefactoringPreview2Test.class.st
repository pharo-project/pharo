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
	sycRefactoringPreview table
		items:
			{MockTreeTableItem new.
			MockTreeTableItem new.
			MockTreeTableItem new}
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
SycRefactoringPreview2Test >> testDropListChangeShouldChangeTableItems [
	"this method can't be test because i can't isolate the command"

	self initializeSycRefactoringUsingForScope.
	sycRefactoringPreview scopeDropList selectedIndex: 2.
	self
		assertCollection: sycRefactoringPreview table items
		hasSameElements: {'banane' . 'Tournai Triple'}
]

{ #category : #'initialize-test' }
SycRefactoringPreview2Test >> testInitializationAllCheckBoxDictionarySetTrue [
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
