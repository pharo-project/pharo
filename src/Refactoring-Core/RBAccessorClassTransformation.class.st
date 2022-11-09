"
I am a refactoring operation for creating accessors for all variables.

Example:
Create accessors for all instance variables:

```
RBAccessorClassTransformation 
	model: RBNamespace new className: 'Morph' .
```
Create accessors for all class instance variables:

```
RBAccessorClassTransformation
	model: RBNamespace new className: 'Morph class' .
```
If the class already contains that accessor, I will create another one with a numbered suffix.

"
Class {
	#name : #RBAccessorClassTransformation,
	#superclass : #RBClassRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #accessing }
RBAccessorClassTransformation class >> kind [
	^ 'Transformation'
]

{ #category : #preconditions }
RBAccessorClassTransformation >> preconditions [

	^ self refactorings
		  inject: self emptyCondition
		  into: [ :result :each | result & each preconditions ]
]

{ #category : #accessing }
RBAccessorClassTransformation >> refactorings [
	| class |
	class := self classObjectFor: className asSymbol.
	^  class instanceVariableNames collect: [ :each | 
		RBCreateAccessorsForVariableTransformation
			variable: each class: class classVariable: false ]
]

{ #category : #transforming }
RBAccessorClassTransformation >> transform [
	self refactorings do: [ :each | self performCompositeRefactoring: each ]
]
