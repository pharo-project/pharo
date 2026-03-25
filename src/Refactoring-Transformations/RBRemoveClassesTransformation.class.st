"
Removes a potential non empty class (to your own risk) and reparent its children to the superclass to avoid fully breaking the subclasses.

## Usage:

```
| transformation |
transformation := (RBRemoveClassTransformation
	className: #RBRemoveClassTransformationTest)
	generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

To get the changes

```
| transf |
transf := (RBRemoveClassTransformation
	className: #RBRemoveClassTransformationTest)
	generateChanges.
(StRefactoringPreviewPresenter changes: transf scopes: {RBBrowserEnvironment new}) open
```

To view changes check `StRefactoringPreviewPresenter>>changes:scopes:`

## Preconditions:
- class must exist

"
Class {
	#name : 'RBRemoveClassesTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'classesDictionary',
		'classNames'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'private' }
RBRemoveClassesTransformation class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class' , String cr
	  , 'when it has subclasses'
]

{ #category : 'api' }
RBRemoveClassesTransformation class >> classNames: aCollection [

	^ self
		model: ReTransformation new model
		classNames: aCollection
]

{ #category : 'api' }
RBRemoveClassesTransformation class >> model: aRBModel classNames: aCollection [

	^ self new
		  model: aRBModel;
		  classNames: aCollection;
		  yourself
]

{ #category : 'preconditions' }
RBRemoveClassesTransformation >> applicabilityPreconditions [

	^ {
		  (ReClassesExistCondition new classes: classesDictionary).
		  self preconditionAreNotMeta }
]

{ #category : 'preconditions' }
RBRemoveClassesTransformation >> breakingChangePreconditions [

	^ { self preconditionHaveNoReferences }
]

{ #category : 'accessing' }
RBRemoveClassesTransformation >> classNames [

	^ classNames
]

{ #category : 'accessing' }
RBRemoveClassesTransformation >> classNames: aClassNameCollection [

	classNames := aClassNameCollection
]

{ #category : 'preconditions' }
RBRemoveClassesTransformation >> preconditionAreNotMeta [

	^ ReClassesAreNotMetaClassCondition new
		  classes: classesDictionary;
		  message: 'Metaclasses can be removed. Remove their class.'
]

{ #category : 'preconditions' }
RBRemoveClassesTransformation >> preconditionHaveNoReferences [

	^ ReClassesHaveNoReferencesCondition new
		  model: model;
		  classes: classesDictionary values 
]

{ #category : 'preconditions' }
RBRemoveClassesTransformation >> preconditions [
	"We hope in the future to push up this method to RBRefactoring"

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBRemoveClassesTransformation >> prepareForExecution [

	classesDictionary := (classNames collect: [ :cl | cl -> (self model classNamed: cl) ]) asDictionary
]

{ #category : 'executing' }
RBRemoveClassesTransformation >> privateTransform [

	self
		reparentSubclasses;
		removeClasses
]

{ #category : 'removing' }
RBRemoveClassesTransformation >> removeClassChanges [
	"We know that the preconditions are ok! This should only be called by drivers when the preconditions have been met."

	self privateTransform.
	^ self changes
]

{ #category : 'removing' }
RBRemoveClassesTransformation >> removeClasses [

	classNames do: [ :each | self model removeClassNamed: each ]
]

{ #category : 'private' }
RBRemoveClassesTransformation >> reparentSubclasses [

	classNames do: [ :each |
			| cls |
			cls := self model classNamed: each.
			self model reparentClasses: cls subclasses copy to: cls superclass ]
]
