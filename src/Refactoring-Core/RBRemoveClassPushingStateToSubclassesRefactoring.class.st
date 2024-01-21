"
I am a refactoring for removing classes but pushing the removed class state (variables and methods) to subclasses.

My precondition verifies that the class name exists in this namespace and the class has no references, resp. users, if this is used to remove a trait.


## Usage 

```
| transf | 
transf := (RBRemoveClassPushingStateToSubclassesRefactoring
	classNames: { #RBTransformationRuleTestData1} )
	generateChanges.
(StRefactoringPreviewPresenter changes: transf scopes: {RBBrowserEnvironment new}) open
```
"
Class {
	#name : 'RBRemoveClassPushingStateToSubclassesRefactoring',
	#superclass : 'RBRemoveClassRefactoring',
	#instVars : [
		'refactorings'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBRemoveClassPushingStateToSubclassesRefactoring >> breakingChangePreconditions [

	^ { self preconditionHaveNoReferences }
]

{ #category : 'transforming' }
RBRemoveClassPushingStateToSubclassesRefactoring >> createRefactoringPushDownClassVarsOf: class [
	class classVariableNames do: [ :e |
		refactorings add: 
			(RBPushDownClassVariableRefactoring model: self model variable: e class: class).
	]
]

{ #category : 'transforming' }
RBRemoveClassPushingStateToSubclassesRefactoring >> createRefactoringPushDownInstVarsOf: class [
	class instanceVariableNames
		do: [ :e |
			refactorings
				add:
					(RBPushDownInstanceVariableRefactoring
						model: self model variable: e
						class: class) ]
]

{ #category : 'transforming' }
RBRemoveClassPushingStateToSubclassesRefactoring >> createRefactoringPushDownMethodsOf: class [
	refactorings add: (RBPushDownMethodRefactoring model: self model pushDown: class selectors from: class)
]

{ #category : 'transforming' }
RBRemoveClassPushingStateToSubclassesRefactoring >> createRefactorings [
	"I mimick a composite refactoring. In the future I will take advantage of the new cool RBCompositeRefactoring."
	
	classNames do:
			[:each |
			| class |
			class := self model classNamed: each.
			self createRefactoringPushDownMethodsOf: class.
			self createRefactoringPushDownInstVarsOf: class.
			self createRefactoringPushDownClassVarsOf: class]
]

{ #category : 'initialization' }
RBRemoveClassPushingStateToSubclassesRefactoring >> initialize [
	super initialize.
	refactorings := OrderedCollection new
]

{ #category : 'transforming' }
RBRemoveClassPushingStateToSubclassesRefactoring >> privateTransform [
	self createRefactorings.
	refactorings do: [ :ref | ref generateChanges ].
	self reparentSubclasses.
	self removeClasses
]

{ #category : 'transforming' }
RBRemoveClassPushingStateToSubclassesRefactoring >> removeClasses [
	classNames do: [:each | self model removeClassKeepingSubclassesNamed: each]
]

{ #category : 'removing' }
RBRemoveClassPushingStateToSubclassesRefactoring >> removeClassesChanges [

	self privateTransform.
	
	^ self changes
]
