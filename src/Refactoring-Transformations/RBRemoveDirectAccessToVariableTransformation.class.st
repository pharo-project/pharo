"
Replaces all the direct accesses to this variable in this class (and its metaclass) by invocation to this variable's getter and setter. It assumes that both getter and setter are implemented in the class.

Usage:
```
| transformation |
transformation := (RBRemoveDirectAccessToVariableTransformation
	instanceVariable: 'environment'
	class: #RBNamespace)
	generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the class must exist
- the class defines a variable with the given name
- the class defines both getter and setter for this variable

"
Class {
	#name : 'RBRemoveDirectAccessToVariableTransformation',
	#superclass : 'RBVariableTransformation',
	#instVars : [
		'getterMethod',
		'setterMethod',
		'receiver'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBRemoveDirectAccessToVariableTransformation >> applicabilityPreconditions [

	| conds |
	class := self model classObjectFor: className.
	conds := isClassVariable
		         ifTrue: [
			         {
				         (RBCondition isMetaclass: class) not.
				         (RBCondition
					          directlyDefinesClassVariable: variableName asSymbol
					          in: class).
				         ((RBCondition withBlock: [
					           (#( #Object #Behavior #ClassDescription
					               #Class ) includes: class name) not ]) errorMacro:
					          'This refactoring does not work for Object, Behavior, ClassDescription, or Class') } ]
		         ifFalse: [
			         { (RBCondition
				          directlyDefinesInstanceVariable: variableName
				          in: class) } ].
	^ conds , 
		{ (RBCondition
		   canUnderstand: self getterMethodSelector
		   in: self definingClass).
		(RBCondition
		   canUnderstand: self setterMethodSelector
		   in: self definingClass)}
]

{ #category : 'accessing' }
RBRemoveDirectAccessToVariableTransformation >> definingClass [
	"For this refactoring the defining class is the class in which the refactoring should be applied.
	This is basically the basic defining class"
	
	^ isClassVariable
		ifTrue: [ super definingClass classSide ]
		ifFalse: [ super definingClass ]
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> getterMethod [

	^ getterMethod ifNil: [
		getterMethod := (self definingClass getterMethodFor: variableName)
			ifNil: [ variableName uncapitalized asSymbol ] ]
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> getterMethodSelector [

	^ getterMethod ifNil: [
		getterMethod := (self definingClass getterMethodFor: variableName)
			ifNil: [ variableName uncapitalized asSymbol ] ]
]

{ #category : 'executing' }
RBRemoveDirectAccessToVariableTransformation >> privateTransform [

	self replaceInstanceAccesses.
	isClassVariable ifTrue: [ self replaceClassAccesses ]
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> receiver [

	^ receiver ifNil: [ self receiver: 'self'. receiver ]
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> receiver: aString [

	receiver := aString
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> replaceClassAccesses [
	"replace accesses in the metaclass"
	
	| replacer |
	replacer := self parseTreeRewriterClass
		variable: variableName
		getter: self getterMethodSelector
		setter: self setterMethodSelector
		receiver: self receiver.
	self model
		convertClasses: self definingClass classSide withAllSubclasses
		select: [ :aClass |
			(aClass whichSelectorsReferToClassVariable: variableName)
				reject: [ :each |
					aClass == self definingClass classSide
						and: [ each == self getterMethodSelector or: [ each == self setterMethodSelector ] ] ] ]
		using: replacer.

	"replace accessed in the class"
	replacer := self parseTreeRewriterClass
		variable: variableName
		getter: 'class ' , self getterMethodSelector
		setter: 'class ' , self setterMethodSelector.
	self model
		convertClasses: self definingClass instanceSide withAllSubclasses
		select: [ :aClass | aClass whichSelectorsReferToClassVariable: variableName ]
		using: replacer
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> replaceInstanceAccesses [

	| replacer includeGettersAndSetters |
	includeGettersAndSetters := receiver isNotNil.
	replacer := self parseTreeRewriterClass
				variable: variableName
				getter: self getterMethodSelector
				setter: self setterMethodSelector
				receiver: self receiver.

	self model
		convertClasses: self definingClass withAllSubclasses
		select: [ :aClass |
			| allReferences |
			allReferences := aClass whichSelectorsReferToInstanceVariable: variableName.
			includeGettersAndSetters
				ifTrue: [ allReferences ]
				ifFalse: [ allReferences
					reject: [ :each |
						aClass == self definingClass
						and: [ each == self getterMethodSelector
						or: [ each == self setterMethodSelector ] ] ] ] ]
		using: replacer
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> setterMethod [

	^ setterMethod ifNil: [
		setterMethod := (self definingClass setterMethodFor: variableName)
			ifNil: [ 	(variableName uncapitalized, ':') asSymbol ] ]
]

{ #category : 'private' }
RBRemoveDirectAccessToVariableTransformation >> setterMethodSelector [

	^ setterMethod ifNil: [
		setterMethod := (self definingClass setterMethodFor: variableName)
			ifNil: [ 	(variableName uncapitalized, ':') asSymbol ] ]
]
