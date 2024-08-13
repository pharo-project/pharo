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
ReDuplicateClassDriverTest >> classToBeDeleted [

	^ #ReClassCopiedToBeDeleted
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> classToBeDuplicated [
	"Answer the <Class> which will be duplicated"
	
	^ ReClassToBeDuplicated 
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> setUpMocksOn: driver [

	| dialog requestClass |
	dialog := MockObject new.
	dialog
		on: #window respond: SpDialogWindowPresenter new beOk;
		on:	 #newClassName respond: #ReClassCopiedToBeDeleted;
		on: #isCancelled respond: true;
		on: #openModal respond: dialog.
	driver requestDialog: dialog.
	driver configureRefactoring.
	
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

	(testingEnvironment notNil and: [ testingEnvironment classNames includes: self classToBeDeleted ])
		ifTrue: [ testingEnvironment removeClass: self classToBeDeleted ].
	super tearDown.
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> testDuplicateClass [

	| driver rbClass driverChanges refactoring methodChanges |

	testingEnvironment := RBClassEnvironment class: self classToBeDuplicated.
	driver := ReDuplicateClassDriver basicNew.

	self setUpMocksOn: driver.

	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment classes anyOne.
	
	refactoring := ReCopyClassRefactoring new.
	refactoring	className: self classToBeDeleted.
	refactoring sourceClass: rbClass.
	driver runRefactoring: refactoring.
	driverChanges := driver refactoring changes.
	self 
		assert: driverChanges changes size 
		equals: 5.

	methodChanges := driverChanges changes select: [ : amc | amc isKindOf: RBAddMethodChange ].
	self 
		assert: (methodChanges select: [ : mc | mc selector = #hash or: [ mc selector = #= ] ]) size
		equals: 2.

]
