"
I am a refactoring for moving a method down to all direct subclasses.

My preconditions verify that this method isn't refered  as a super send in the subclass. And the class defining this method is abstract or not referenced anywhere.


"
Class {
	#name : 'RePushDownMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selectors',
		'classes'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
RePushDownMethodRefactoring class >> model: anEnvironment pushDown: selectorCollection from: aClassName [
	^ self new
		model: anEnvironment;
		pushDown: selectorCollection from: aClassName;
		yourself
]

{ #category : 'instance creation' }
RePushDownMethodRefactoring class >> model: anEnvironment pushDown: selectorCollection from: aClass in: classes [
	^ self new
		model: anEnvironment;
		pushDown: selectorCollection from: aClass in: classes;
		yourself
]

{ #category : 'instance creation' }
RePushDownMethodRefactoring class >> pushDown: selectorCollection from: aClass [
	^ self new
		pushDown: selectorCollection from: aClass
]

{ #category : 'instance creation' }
RePushDownMethodRefactoring class >> pushDown: selectorCollection from: aClass in: classes [
	^ self new
		pushDown: selectorCollection from: aClass in: classes
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> allClasses [

	^ classes ifNil: [ class subclasses ]
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> applicabilityPreconditions [
	"Check that all selectors are defined in `class`"

	^ { (ReDefinesSelectorsCondition new
		   definesSelectors: selectors
		   in: class) }
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> breakingChangePreconditions [
	"Check that that none of the subclasses of `class` is doing a supercall to any of the selectors
	that will be pushed down.
	
	Also, to ensure that an instance of the class is not sent a message which is pushed down,  forces that 
	we can only push down methods from abstract class. 
	This should be controlled via a flag on the ui.
	"

	^ OrderedCollection
		  with: self preconditionIsAbstract
		  with: self preconditionSubclassesDontSendSuper
		  with: self preconditionSubclassesDirectlyDefines
		  with: self preconditionMethodsReferToSelector
		  with: self preconditionAllSubclassesDirectlyDefines
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> classes: aCollection [
	classes := aCollection collect: [ :cls |
		self classObjectFor: cls.
	]
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> preconditionAllSubclassesDirectlyDefines [

	^ ReAllSubclassesDirectlyDefinesCondition new
		  class: class
		  selectors: selectors 
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> preconditionIsAbstract [

	^ ReClassesAreAbstractCondition new classes: { class }
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> preconditionMethodsReferToSelector [

	^ ReMethodsReferToSelectorCondition new
		  class: class
		  selectors: selectors 
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> preconditionSubclassesDirectlyDefines [

	^ ReSubclassesDirectlyDefinesCondition new class: class selectors: selectors
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> preconditionSubclassesDontSendSuper [

	^ ReSubclassesDontSendSuperCondition new
		  class: class
		  selectors: selectors
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
	
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> privateTransform [

	selectors do: [ :each | self pushDown: each ].
	selectors do: [ :each |
		self generateChangesFor:
			(RBRemoveMethodTransformation selector: each from: class) ]
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> pushDown: aSelector [

	| code protocols refactoring |
	code := class sourceCodeFor: aSelector.
	protocols := class protocolsFor: aSelector.
	refactoring := RBExpandReferencedPoolsRefactoring
		               model: self model
		               forMethod: (class parseTreeForSelector: aSelector)
		               fromClass: class
		               toClasses: self allClasses.
	self generateChangesFor: refactoring.
	self allClasses do: [ :each |
		self generateChangesFor: (RBAddMethodTransformation sourceCode: code in: each withProtocol: protocols) ]
]

{ #category : 'initialization' }
RePushDownMethodRefactoring >> pushDown: selectorCollection from: aClassName [
	class := self model classNamed: aClassName.
	selectors := selectorCollection
]

{ #category : 'initialization' }
RePushDownMethodRefactoring >> pushDown: selectorCollection from: aClass in: aCollection [
	class := self classObjectFor: aClass.
	selectors := selectorCollection.
	self classes: aCollection
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> pushDownMethodChanges [

	self privateTransform.
	^ self changes
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> selectors [
	^ selectors
]

{ #category : 'transforming' }
RePushDownMethodRefactoring >> selectors: aList [
	selectors := aList
]

{ #category : 'preconditions' }
RePushDownMethodRefactoring >> targetClass [
	^ class
]
