"
I am a refactoring for removing classes. 

My precondition verifies that the class name exists in this namespace and the class has no references (direct references only! pay attention we don't check for symbols representing the class!!!).

If this class is ""empty"" (has no methods and no variables), any subclass is reparented to the superclass of this class. It is not allowed to remove non-empty classes when it has subclasses.
"
Class {
	#name : 'RBRemoveClassRefactoring',
	#superclass : 'RBRefactoring',
	#instVars : [
		'classNames',
		'classesDictionary'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'error messages' }
RBRemoveClassRefactoring class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class' , String cr
	  , 'when it has subclasses'
]

{ #category : 'instance creation' }
RBRemoveClassRefactoring class >> classNames: aClassNameCollection [
	^ self new
		classNames: aClassNameCollection
]

{ #category : 'instance creation' }
RBRemoveClassRefactoring class >> model: aRBSmalltalk classNames: aClassNameCollection [
	^ self new
		model: aRBSmalltalk;
		classNames: aClassNameCollection;
		yourself
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> applicabilityPreconditions [

	| conditions |
	conditions := self trueCondition.
	classesDictionary keysAndValuesDo: [ :className :class |
		conditions & (RBCondition
			 withBlock: [ class isNotNil ]
			 errorString: 'No such class or trait ' , className)
		& (self preconditionIsNotMetaclass: class) ].
	^ conditions
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> breakingChangePreconditions [

	^ ((RBCondition withBlock: [ self preconditionEmptyClasses check ] errorString: 'Some classes are not empty')
	  & (RBCondition withBlock: [ self preconditionHaveNoSubclasses check ] errorString: 'Some classes have subclasses'))
	  & (RBCondition withBlock: [ self preconditionHaveNoReferences check ] errorString: 'Some classes still have references')
]

{ #category : 'accessing' }
RBRemoveClassRefactoring >> classNames [
	^ classNames
]

{ #category : 'initialization' }
RBRemoveClassRefactoring >> classNames: aClassNameCollection [
	classNames := aClassNameCollection
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> environmentWithUsersOf: aClassable [
	^ RBClassEnvironment
		onEnvironment: RBBrowserEnvironment new
		classes: (self model classesReferencingClass: aClassable)
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> preconditionEmptyClasses [

	^ RBClassesEmptyCondition new classes: classesDictionary
]

{ #category : 'helper methods' }
RBRemoveClassRefactoring >> preconditionHasNoUsers: aClassOrTrait [

	^ (RBCondition withBlock: [
				(self refactoringAwareUsesOf: aClassOrTrait) isEmpty ])
		  errorMacro:
			  aClassOrTrait name , ' Trait is used.<n>Browse classes using it?';
		  errorBlock: [
			  self openBrowserOn: (self environmentWithUsersOf: aClassOrTrait) ];
		  yourself
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> preconditionHaveNoReferences [

	^ RBClassesHaveNoReferencesCondition new
		  model: model;
		  classes: classesDictionary values
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> preconditionHaveNoSubclasses [

	^ RBClassesHaveNoSubclassesCondition new classes: classesDictionary					
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> preconditionIsNotMetaclass: aClass [
	^ ((RBCondition isMetaclass: aClass) errorMacro: 'Cannot remove just the metaclass') not
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> preconditions [
	"We hope in the future to push up this method to RBRefactoring"

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBRemoveClassRefactoring >> prepareForExecution [

	classesDictionary := (classNames collect: [ :className |
		            className -> (self model classNamed: className asSymbol) ])
		           asDictionary
]

{ #category : 'preparation' }
RBRemoveClassRefactoring >> prepareForInteractiveMode [

	self prepareForExecution.
]

{ #category : 'transforming' }
RBRemoveClassRefactoring >> privateTransform [
	self
		reparentSubclasses;
		removeClasses
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> refactoringAwareUsesOf: aClass [
	"Return the 'symbol use' to the class taking into account that some uses may be created by other classes that should be removed by the refactoring."
	
^ ((self model allReferencesTo: aClass name)
		collect: [ :each | each methodClass name ]) copyWithoutAll: classNames
]

{ #category : 'removing' }
RBRemoveClassRefactoring >> removeClassChanges [

	self privateTransform.

	^ self changes
]

{ #category : 'transforming' }
RBRemoveClassRefactoring >> removeClasses [
	classNames do: [:each | self model removeClassNamed: each]
]

{ #category : 'transforming' }
RBRemoveClassRefactoring >> reparentSubclasses [
	classNames do:
			[:each |
			| class |
			class := self model classNamed: each.
			self model reparentClasses: class subclasses copy to: class superclass]
]

{ #category : 'private' }
RBRemoveClassRefactoring >> senders [

	^ classNames flatCollect: [ :className |
		  | aClass |
		  aClass := self model classNamed: className asSymbol.
		  self refactoringAwareReferencesTo: aClass ]
]

{ #category : 'printing' }
RBRemoveClassRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' classNames: '.
	classNames asArray storeOn: aStream.
	aStream nextPut: $)
]
