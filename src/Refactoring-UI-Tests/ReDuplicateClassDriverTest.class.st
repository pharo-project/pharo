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
ReDuplicateClassDriverTest >> classToBeDuplicated [
	"Answer the <Class> which will be duplicated"
	
	^ ReClassToBeDuplicated 
]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> setUpMocksOn: driver [

	| dialog |
	dialog := MockObject new.
	dialog
		on: #window respond: SpDialogWindowPresenter new beOk;
		on:	 #newClassName respond: #ReClassCopiedToBeDeleted;
		on: #isCancelled respond: true;
		on: #openModal respond: dialog.
	driver requestDialog: dialog.
	driver configureRefactoring.

]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> testDuplicateClass [

	| driver rbClass driverChanges |

	testingEnvironment := RBClassEnvironment class: self classToBeDuplicated.
	driver := ReDuplicateClassDriver basicNew.

	self setUpMocksOn: driver.

	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment class.
	
	driver refactoring	className: rbClass.
	driver runRefactoring.
	driverChanges := driver refactoring changes.
	self 
		assert: driverChanges changes size 
		equals: 4.
	self assertEmpty: driver refactoring failedApplicabilityPreconditions.
	self 
		assert: (driverChanges changes select: [ : amc | amc selector = #hash or: [ amc selector = #= ] ]) size
		equals: 2.

	testingEnvironment ifNotNil: [ 
		testingEnvironment classesDo: [ : cls |
			(cls includesSelector: #=)
				ifTrue: [ cls removeSelector: #= ].
			(cls includesSelector: #hash)
				ifTrue: [ cls removeSelector: #hash ] ] ].

]
