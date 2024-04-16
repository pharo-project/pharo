"
I am a refactoring for removing classes. 

My precondition verifies that the class name exists in this namespace and the class has no references (direct references only! pay attention we don't check for symbols representing the class!!!).

If this class is ""empty"" (has no methods and no variables), any subclass is reparented to the superclass of this class. It is not allowed to remove non-empty classes when it has subclasses.
"
Class {
	#name : 'ReRemoveClassRefactoring',
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
ReRemoveClassRefactoring class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class' , String cr
	  , 'when it has subclasses'
]

{ #category : 'instance creation' }
ReRemoveClassRefactoring class >> classNames: aClassNameCollection [
	^ self new
		classNames: aClassNameCollection
]

{ #category : 'instance creation' }
ReRemoveClassRefactoring class >> model: aRBSmalltalk classNames: aClassNameCollection [
	^ self new
		model: aRBSmalltalk;
		classNames: aClassNameCollection;
		yourself
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> applicabilityPreconditions [

	^ {
		  (ReClassesExistCondition new classes: classesDictionary).
		  self preconditionAreNotMeta }
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> breakingChangePreconditions [
	"Empty error blocks are here so that warnings are raised. This is a ugly hack..."

	^ {
		  self preconditionHaveNoReferences.
		  (self preconditionHaveNoSubclasses | self preconditionEmptyClasses) }
]

{ #category : 'scripting api - conditions' }
ReRemoveClassRefactoring >> checkPreconditions [

	self eagerlyCheckApplicabilityPreconditions.
	self checkBreakingChangePreconditions
]

{ #category : 'accessing' }
ReRemoveClassRefactoring >> classNames [
	^ classNames
]

{ #category : 'initialization' }
ReRemoveClassRefactoring >> classNames: aClassNameCollection [
	classNames := aClassNameCollection
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> environmentWithUsersOf: aClassable [
	^ RBClassEnvironment
		onEnvironment: RBBrowserEnvironment new
		classes: (self model classesReferencingClass: aClassable)
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> preconditionAreNotMeta [

	^ ReClassesAreNotMetaClassCondition new classes: classesDictionary
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> preconditionEmptyClasses [

	^ ReClassesEmptyCondition new classes: classesDictionary
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> preconditionHaveNoReferences [

	^ ReClassesHaveNoReferencesCondition new
		  model: model;
		  classes: classesDictionary values
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> preconditionHaveNoSubclasses [

	^ ReClassesHaveNoSubclassesCondition new classes: classesDictionary					
]

{ #category : 'preconditions' }
ReRemoveClassRefactoring >> preconditions [
	"We hope in the future to push up this method to RBRefactoring"

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
ReRemoveClassRefactoring >> prepareForExecution [

	classesDictionary := (classNames collect: [ :className |
		            className -> (self model classNamed: className) ])
		           asDictionary
]

{ #category : 'preparation' }
ReRemoveClassRefactoring >> prepareForInteractiveMode [

	self prepareForExecution.
]

{ #category : 'transforming' }
ReRemoveClassRefactoring >> privateTransform [
	self
		reparentSubclasses;
		removeClasses
]

{ #category : 'removing' }
ReRemoveClassRefactoring >> removeClassChanges [
	"We know that the preconditions are ok! This should only be called by drivers when the preconditions have been met."
	
	
	self privateTransform.
	^ self changes
]

{ #category : 'transforming' }
ReRemoveClassRefactoring >> removeClasses [
	classNames do: [:each | self model removeClassNamed: each]
]

{ #category : 'transforming' }
ReRemoveClassRefactoring >> reparentSubclasses [
	classNames do:
			[:each |
			| class |
			class := self model classNamed: each.
			self model reparentClasses: class subclasses copy to: class superclass]
]

{ #category : 'storing' }
ReRemoveClassRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' classNames: '.
	classNames asArray storeOn: aStream.
	aStream nextPut: $)
]
