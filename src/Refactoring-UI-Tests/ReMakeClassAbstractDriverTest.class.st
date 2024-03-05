Class {
	#name : 'ReMakeClassAbstractDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReMakeClassAbstractDriverTest >> classWithReferenceToAbstract [

	^ RBWithSelfClassReferenceTest.
]

{ #category : 'tests' }
ReMakeClassAbstractDriverTest >> classWithoutReferenceToAbstract [

	^ RBClassWithoutSelfClassReferenceTest.
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

	self classWithoutReferenceToAbstract class removeSelector: #isAbstract.
	super tearDown.
]

{ #category : 'tests' }
ReMakeClassAbstractDriverTest >> testMakeClassAbstractWhenClassIsReferenced [
	"Test making a class abstract when it is referenced"

	| driver environment rbClass |

	environment := RBClassEnvironment class: self classWithReferenceToAbstract.
	driver := RBMakeClassAbstractDriver new
			scopes: { environment } class: self classWithReferenceToAbstract;
			yourself.

	rbClass := driver model classFor: self classWithReferenceToAbstract.

	self 
		deny: rbClass isAbstract
		description: 'It tests that the RB class is not abstract because it contains its class reference'.
		
	self setUpDriver: driver.
	driver runRefactoring.
	
	self 
		deny: rbClass isAbstract
		description: 'It tests that the RB class is still abstract after applying the refactoring'.
]

{ #category : 'tests' }
ReMakeClassAbstractDriverTest >> testMakeClassAbstractWhenClassNotUsed [
	"Test making a class abstract when it is not used anywhere in the system"
	| driver environment rbClass |

	environment := RBClassEnvironment class: self classWithoutReferenceToAbstract.
	driver := RBMakeClassAbstractDriver new
			scopes: { environment } class: self classWithoutReferenceToAbstract;
			yourself.

	rbClass := driver model classFor: self classWithoutReferenceToAbstract.

	self 
		assert: rbClass isAbstract
		description: 'It tests that the RB class is abstract by default'.
		
	driver runRefactoring.

	self 
		assert: (self classWithoutReferenceToAbstract class respondsTo: #isAbstract)
		description: 'It test that we successfully applied the refactoring which adds #isAbstract to the target class'.
		
	self 
		assert: rbClass isAbstract
		description: 'It tests that the RB class is still abstract after applying the refactoring'.
]
