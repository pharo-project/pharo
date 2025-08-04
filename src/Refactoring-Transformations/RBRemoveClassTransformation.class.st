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
	#name : 'RBRemoveClassTransformation',
	#superclass : 'RBClassTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'private' }
RBRemoveClassTransformation class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class' , String cr
	  , 'when it has subclasses'
]

{ #category : 'api' }
RBRemoveClassTransformation class >> classNames: aCollection [

	^ self
		model: ReTransformation new model
		classNames: aCollection
]

{ #category : 'api' }
RBRemoveClassTransformation class >> model: aRBModel classNames: aCollection [

	^ RBCompositeTransformation new
		transformations: (aCollection
			collect: [ :e | self model: aRBModel className: e ]);
		yourself
]

{ #category : 'preconditions' }
RBRemoveClassTransformation >> applicabilityPreconditions [

	| aClassOrTrait |
	aClassOrTrait := self model classNamed: className.
	aClassOrTrait ifNil: [
		self refactoringError: 'No such class or trait named ' , className ].
	^ { (self preconditionIsNotMetaclass: aClassOrTrait) }
]

{ #category : 'preconditions' }
RBRemoveClassTransformation >> preconditionIsNotMetaclass: aClass [

	^ ((RBCondition isMetaclass: aClass)
		errorMacro: 'Cannot remove just the metaclass') not
]

{ #category : 'executing' }
RBRemoveClassTransformation >> privateTransform [

	self
		reparentSubclasses;
		removeClass
]

{ #category : 'private' }
RBRemoveClassTransformation >> removeClass [

	self model removeClassNamed: className
]

{ #category : 'private' }
RBRemoveClassTransformation >> reparentSubclasses [

	self model
		reparentClasses: self definingClass subclasses copy
		to: self definingClass superclass
]

{ #category : 'storing' }
RBRemoveClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' className: '.
	className storeOn: aStream.
	aStream nextPut: $)
]
