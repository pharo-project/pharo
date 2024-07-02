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
		on: #presenter respond: dialog;
		on: #packageName respond: self classToBeDuplicated packageName;
		on: #tagName respond: #ForDuplication;
		on:	 #newClassName respond: #ReClassCopiedToBeDeleted;
		on: #packageName respond: self classToBeDuplicated packageName;
		on: #tagName respond: #ForDuplication;
		on:	 #newClassName respond: #ReClassCopiedToBeDeleted;
		on: #selectedInstanceMethods respond: #();
		on: #selectedClassMethods respond: #();
		on: #isCancelled respond: true;
		on: #openModal respond: dialog.
	driver requestDialog: dialog.
	driver configureRefactoring.

]

{ #category : 'tests' }
ReDuplicateClassDriverTest >> testDuplicateClass [

	| driver driverChanges |
	
	testingEnvironment := RBClassEnvironment class: self classToBeDuplicated.
	driver := ReDuplicateClassDriver basicNew.
	
	driver className: self classToBeDuplicated name.
	driver scopes: { testingEnvironment }.
	
	self setUpMocksOn: driver.
	
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
