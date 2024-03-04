Class {
	#name : 'ReMakeClassAbstractDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'model',
		'environment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReMakeClassAbstractDriverTest >> classToAbstract [

	^ MyClassA.
]

{ #category : 'initialization' }
ReMakeClassAbstractDriverTest >> setUpDriver: driver [

	| dialog |
	super setUpDriver: driver.
	dialog := MockObject new.
	dialog
		on: #openModal
		respond: true.
	driver requestDialog: dialog.
]

{ #category : 'running' }
ReMakeClassAbstractDriverTest >> tearDown [

	self classToAbstract class removeSelector: #isAbstract.
	super tearDown.
]

{ #category : 'tests' }
ReMakeClassAbstractDriverTest >> testMakeClassAbstractWhenClassNotUsed [
	"Test making a class abstract when it is not used anywhere in the system"
	| driver |

	environment := RBClassEnvironment class: self classToAbstract.
	model := RBNamespace onEnvironment: environment.
	
	driver := RBMakeClassAbstractDriver new
			class: self classToAbstract;
			scopes: { environment };
			yourself.
	self setUpDriver: driver.
		
	driver runRefactoring.
	self assert: (self classToAbstract class respondsTo: #isAbstract).
	self assert: self classToAbstract isAbstract.
]
