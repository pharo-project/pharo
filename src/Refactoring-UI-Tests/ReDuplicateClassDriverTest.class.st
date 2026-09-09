Class {
	#name : 'ReDuplicateClassDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReDuplicateClassDriverTest >> newName [

	^ #ReClassCopiedToBeDeleted
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> setUpMocksOn: driver [

	| dialog requestClass selectedSuperclass |
	selectedSuperclass := MockObject new.
	selectedSuperclass on: #name respond: ReClassToBeDuplicated superclass name.

	dialog := MockObject new.
	dialog
		on: #window respond: SpDialogWindowPresenter new beOk;
		on: #selectedSuperclass respond: selectedSuperclass;
		on:	 #newClassName respond: #ReClassCopiedToBeDeleted;
		on: #packageName respond: #SomePackage;
		on: #isCancelled respond: true;
		on: #openModal respond: dialog;
		on: #packageName respond: #SomePackage.
	driver requestDialog: dialog.
	
	requestClass := MockObject new.
	requestClass
		on: #withChanges:scopes: respond: dialog;
		on: #application: respond: dialog;
		on: #refactoring: respond: dialog;
		on: #openModal respond: dialog.		
	driver previewPresenterClass: requestClass.
]

{ #category : 'running' }
ReDuplicateClassDriverTest >> tearDown [ 

	(testingEnvironment notNil and: [ testingEnvironment classNames includes: self newName ])
		ifTrue: [ testingEnvironment removeClass: self newName ].
	super tearDown.
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> testDuplicateClass [
	| driver rbClass driverChanges methodChanges |
	
	testingEnvironment := RBClassEnvironment classes: {
			                      ReClassToBeDuplicated.
			                      ReClassToBeDuplicated superclass }.
	
	driver := ReDuplicateClassDriver className: ReClassToBeDuplicated name.

	self setUpMocksOn: driver.

	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment classes anyOne.

	driver runRefactoring.
	driverChanges := driver refactoring changes.
	self assert: driverChanges changes size equals: 5.

	methodChanges := driverChanges changes select: [ :amc |
		                 amc isKindOf: ReAddMethodChange ].
	self
		assert: (methodChanges select: [ :mc |
				 mc selector = #hash or: [ mc selector = #= ] ]) size
		equals: 2
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> testDuplicateClassInIncompleteModelFails [

	| driver rbClass |
	testingEnvironment := RBClassEnvironment classes: { ReClassToBeDuplicated }.
	driver := ReDuplicateClassDriver className:
		          ReClassToBeDuplicated name.

	self setUpMocksOn: driver.

	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment classes anyOne.

	self
		should: [ driver runRefactoring ]
		raise: RBRefactoringError
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> testDuplicateClassWithTrait [

	| driver rbClass driverChanges methodChanges classAddition |
	testingEnvironment := RBClassEnvironment classes: {
			                      ReClassToBeDuplicatedWithTraits.
			                      ReClassToBeDuplicatedWithTraits superclass }.
	driver := ReDuplicateClassDriver className:
		          ReClassToBeDuplicatedWithTraits name.

	self setUpMocksOn: driver.

	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment classes anyOne.

	driver runRefactoring.
	driverChanges := driver refactoring changes.
	self assert: driverChanges changes size equals: 5.

	methodChanges := driverChanges changes select: [ :amc |
		                 amc isKindOf: ReAddMethodChange ].
	self
		assert: (methodChanges select: [ :mc |
				 mc selector = #hash or: [ mc selector = #= ] ]) size
		equals: 2.

	classAddition := driverChanges changes detect: [ :c |
		                 c isKindOf: ReAddClassChange ].
	self
		assert: classAddition traitComposition asString
		equals: ReClassToBeDuplicatedWithTraits traitCompositionString
]
