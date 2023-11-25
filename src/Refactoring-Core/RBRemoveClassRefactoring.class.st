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

	^ { RBClassesExistCondition new classes: classesDictionary . self preconditionAreNotMeta }
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> breakingChangePreconditions [
	"Empty error blocks are here so that warnings are raised. This is a ugly hack..."

	^ { self preconditionHaveNoReferences .
		 self preconditionHaveNoSubclasses |
		 self preconditionEmptyClasses }
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> checkBreakingChangePreconditions [

	| conditions errorStrings |
	conditions := self breakingChangePreconditions.
	conditions := conditions reject: [ :cond | cond check ].
	conditions ifEmpty: [ ^ self ].
	errorStrings := String streamContents: [ :aStream |
		                conditions do: [ :cond |
			                cond violationMessageOn: aStream ] ].
	self refactoringWarning: errorStrings
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> checkPreconditions [

	self eagerlyCheckApplicabilityPreconditions.
	self checkBreakingChangePreconditions
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
RBRemoveClassRefactoring >> eagerlyCheckApplicabilityPreconditions [

	self applicabilityPreconditions
		do: [ :cond | cond check ifFalse:
				[ self refactoringError:
					(String streamContents: [ :aStream | cond violationMessageOn: aStream ] ) ] ]
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> environmentWithUsersOf: aClassable [
	^ RBClassEnvironment
		onEnvironment: RBBrowserEnvironment new
		classes: (self model classesReferencingClass: aClassable)
]

{ #category : 'preconditions' }
RBRemoveClassRefactoring >> preconditionAreNotMeta [

	^ RBClassesAreNotMetaClassCondition new classes: classesDictionary
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
RBRemoveClassRefactoring >> preconditions [
	"We hope in the future to push up this method to RBRefactoring"

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBRemoveClassRefactoring >> prepareForExecution [

	classesDictionary := (classNames collect: [ :className |
		            className -> (self model classNamed: className) ])
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
