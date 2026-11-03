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
	#name : 'ReRemoveClassesTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'classesDictionary',
		'classNames'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'private' }
ReRemoveClassesTransformation class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class' , String cr
	  , 'when it has subclasses'
]

{ #category : 'api' }
ReRemoveClassesTransformation class >> classNames: aCollection [

	^ self
		model: ReTransformation new model
		classNames: aCollection
]

{ #category : 'api' }
ReRemoveClassesTransformation class >> model: aRBModel classNames: aCollection [

	^ self new
		  model: aRBModel;
		  classNames: aCollection;
		  yourself
]

{ #category : 'preconditions' }
ReRemoveClassesTransformation >> applicabilityPreconditions [

	^ {
		  (ReClassesExistCondition new classes: classesDictionary).
		  self preconditionAreNotMeta }
]

{ #category : 'accessing' }
ReRemoveClassesTransformation >> classNames [

	^ classNames
]

{ #category : 'accessing' }
ReRemoveClassesTransformation >> classNames: aClassNameCollection [

	classNames := aClassNameCollection
]

{ #category : 'preconditions' }
ReRemoveClassesTransformation >> preconditionAreNotMeta [

	^ ReClassesAreNotMetaClassCondition new
		  classes: (classesDictionary values reject: [ :eachClass | eachClass isNil ]);
		  message: 'Metaclasses can be removed. Remove their class.'
]

{ #category : 'transforming' }
ReRemoveClassesTransformation >> prepareForExecution [

	classesDictionary := (classNames collect: [ :cl | cl -> (self model classNamed: cl) ]) asDictionary
]

{ #category : 'executing' }
ReRemoveClassesTransformation >> privateTransform [

	self
		reparentSubclasses;
		removeClasses
]

{ #category : 'removing' }
ReRemoveClassesTransformation >> removeClassChanges [
	"We know that the preconditions are ok! This should only be called by drivers when the preconditions have been met."

	self privateTransform.
	^ self changes
]

{ #category : 'removing' }
ReRemoveClassesTransformation >> removeClasses [

	classNames do: [ :each | self model removeClassNamed: each ]
]

{ #category : 'private' }
ReRemoveClassesTransformation >> reparentSubclasses [

	classNames do: [ :each |
			| cls |
			cls := self model classNamed: each.
			self model reparentClasses: cls subclasses copy to: cls superclass ]
]
