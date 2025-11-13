"
Creates accessors for a variable in a class, then replaces all the direct accesses to this variable by invocation to the accessors.

Usage:
```
| transformation |
transformation := (RBProtectVariableTransformation
	instanceVariable: 'class'
	class: #RBTransformationDummyRuleTest)
	generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
overriden from RBAddAccessorsForClassTransformation and RBRemoveDirectAccessToVariableTransformation
"
Class {
	#name : 'RBAbstractInstanceVariableTransformation',
	#superclass : 'RBCompositeVariableTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'executing' }
RBAbstractInstanceVariableTransformation >> buildTransformations [

	^ OrderedCollection
		with: (RBAddVariableAccessorTransformation
				model: self model
				variable: variableName asString
				class: className
				classVariable: isClassVariable)
		with: (RBRemoveDirectAccessToVariableTransformation
				model: self model
				variable: variableName asString
				class: className
				classVariable: isClassVariable) 
]

{ #category : 'storing' }
RBAbstractInstanceVariableTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	className storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	isClassVariable storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'api' }
RBAbstractInstanceVariableTransformation >> variable: aVariableName class: aClassName classVariable: aBoolean [

	variableName := aVariableName.
	isClassVariable := aBoolean.
	className := aClassName
]
