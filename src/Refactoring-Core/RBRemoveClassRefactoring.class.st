"
I am a refactoring for removing classes. 

My precondition verifies that the class name exists in this namespace and the class has no references (direct references or symbols representing the class).

If this class is ""empty"" (has no methods and no variables), any subclass is reparented to the superclass of this class. It is not allowed to remove non-empty classes when it has subclasses.
"
Class {
	#name : #RBRemoveClassRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'classNames'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'error messages' }
RBRemoveClassRefactoring class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class' , String cr
	  , 'when it has subclasses'
]

{ #category : #'instance creation' }
RBRemoveClassRefactoring class >> classNames: aClassNameCollection [
	^ self new
		classNames: aClassNameCollection
]

{ #category : #'instance creation' }
RBRemoveClassRefactoring class >> model: aRBSmalltalk classNames: aClassNameCollection [
	^ self new
		model: aRBSmalltalk;
		classNames: aClassNameCollection;
		yourself
]

{ #category : #initialization }
RBRemoveClassRefactoring >> classNames: aClassNameCollection [
	classNames := aClassNameCollection
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> environmentWithUsersOf: aClassable [
	^ RBClassEnvironment
		onEnvironment: RBBrowserEnvironment new
		classes: (self model classesReferencingClass: aClassable)
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> preconditionEmptyOrHasNoSubclasses: aClass [
	^ (RBCondition hasSubclasses: aClass excluding: classNames) not
		|
		(RBCondition isEmptyClass: aClass)
			
					
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> preconditionHasNoReferences: aClass [
	
	| referencingClassNames |
	referencingClassNames := (self refactoringAwareReferencesTo: aClass).
	^ (RBCondition withBlock: [ referencingClassNames isEmpty ])
			errorMacro: aClass name , ' is referenced.<n>Browse references?';
			errorBlock: [ self openBrowserOn: (RBClassEnvironment classes: referencingClassNames) ];
			yourself
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> preconditionHasNoUsers: aClassOrTrait [

	^ (RBCondition withBlock: [
				(self refactoringAwareUsesOf: aClassOrTrait) isEmpty ])
		  errorMacro:
			  aClassOrTrait name , ' Trait is used.<n>Browse classes using it?';
		  errorBlock: [
			  self openBrowserOn: (self environmentWithUsersOf: aClassOrTrait) ];
		  yourself
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> preconditionIsNotMetaclass: aClass [
	^ ((RBCondition isMetaclass: aClass) errorMacro: 'Cannot remove just the metaclass') not
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> preconditionNoTopClasses: aClass [

	^ RBCondition 
		withBlock: [ aClass superclass notNil ]
		errorString: 'Cannot remove top level class<n>when it has subclasses'
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> preconditions [

	^ classNames inject: self trueCondition into: [ :sum :each |
		  | aClassOrTrait |
		  aClassOrTrait := self model classNamed: each asSymbol.
		  aClassOrTrait ifNil: [
			  self refactoringError: 'No such class or trait' ].
		  sum & ((self preconditionNoTopClasses: aClassOrTrait) 
			& (self preconditionIsNotMetaclass: aClassOrTrait)
		   & (self preconditionHasNoReferences: aClassOrTrait)
		   & (self preconditionEmptyOrHasNoSubclasses: aClassOrTrait)
		   & (self preconditionHasNoUsers: aClassOrTrait)) ] 
]

{ #category : #transforming }
RBRemoveClassRefactoring >> privateTransform [
	self
		reparentSubclasses;
		removeClasses
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> refactoringAwareReferencesTo: aClass [
	"Return the references to the class taking into account that some references may be created by other classes that should be removed by the refactoring."
	
^ ((self model classesReferencingClass: aClass) 
		collect: [ :each | each name ]) copyWithoutAll: classNames
]

{ #category : #preconditions }
RBRemoveClassRefactoring >> refactoringAwareUsesOf: aClass [
	"Return the 'symbol use' to the class taking into account that some uses may be created by other classes that should be removed by the refactoring."
	
^ ((self model allReferencesTo: aClass name)
		collect: [ :each | each methodClass name ]) copyWithoutAll: classNames
]

{ #category : #transforming }
RBRemoveClassRefactoring >> removeClasses [
	classNames do: [:each | self model removeClassNamed: each]
]

{ #category : #transforming }
RBRemoveClassRefactoring >> reparentSubclasses [
	classNames do:
			[:each |
			| class |
			class := self model classNamed: each.
			self model reparentClasses: class subclasses copy to: class superclass]
]

{ #category : #printing }
RBRemoveClassRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' classNames: '.
	classNames asArray storeOn: aStream.
	aStream nextPut: $)
]
