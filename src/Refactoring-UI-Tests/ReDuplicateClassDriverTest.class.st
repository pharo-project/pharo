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

	| dialog requestClass |
	dialog := MockObject new.
	dialog
		on: #window respond: SpDialogWindowPresenter new beOk;
		on:	 #newClassName respond: #ReClassCopiedToBeDeleted;
		on: #packageName respond: #SomePackage;
		on: #isCancelled respond: true;
		on: #openModal respond: dialog.
	driver requestDialog: dialog.
	
	requestClass := MockObject new.
	requestClass
		on: #changes:scopes: respond: dialog;
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
	driver := ReDuplicateClassDriver className:
		          ReClassToBeDuplicated name.

	self setUpMocksOn: driver.

	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment classes anyOne.

	driver runRefactoring.
	driverChanges := driver refactoring changes.
	self assert: driverChanges changes size equals: 5.

	methodChanges := driverChanges changes select: [ :amc |
		                 amc isKindOf: RBAddMethodChange ].
	self
		assert: (methodChanges select: [ :mc |
				 mc selector = #hash or: [ mc selector = #= ] ]) size
		equals: 2
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
		                 amc isKindOf: RBAddMethodChange ].
	self
		assert: (methodChanges select: [ :mc |
				 mc selector = #hash or: [ mc selector = #= ] ]) size
		equals: 2.

	classAddition := driverChanges changes detect: [ :c |
		                 c isKindOf: RBAddClassChange ].
	self
		assert: classAddition traitComposition asString
		equals: ReClassToBeDuplicatedWithTraits traitCompositionString
]
