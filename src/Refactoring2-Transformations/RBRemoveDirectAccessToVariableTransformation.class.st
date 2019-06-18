"
Replaces all the direct accesses to this variable in this class by invocation to this variable's getter and setter. It assumes that both getter and setter are implemented in the class.

Usage:
transformation := (RBRemoveDirectAccessToVariableTransformation
	instanceVariable: 'environment'
	class: #RBNamespace)
	transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class must exist
- the class defines a variable with the given name
- the class defines both getter and setter for this variable

"
Class {
	#name : #RBRemoveDirectAccessToVariableTransformation,
	#superclass : #RBVariableTransformation,
	#instVars : [
		'getterMethod',
		'setterMethod',
		'receiver'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #private }
RBRemoveDirectAccessToVariableTransformation >> getterMethod [

	^ getterMethod ifNil: [ 
		getterMethod := (self definingClass getterMethodFor: variableName)
			ifNil: [ variableName uncapitalized asSymbol ] ]
]

{ #category : #preconditions }
RBRemoveDirectAccessToVariableTransformation >> preconditions [

	class := self model classObjectFor: className.

	^ (isClassVariable
		ifTrue: [ (RBCondition isMetaclass: class) not
			& (RBCondition directlyDefinesClassVariable: variableName asSymbol in: class) 
			& ((RBCondition withBlock: 
				[(#(#Object #Behavior #ClassDescription #Class) includes: class name) not]) 
				errorMacro: 'This refactoring does not work for Object, Behavior, ClassDescription, or Class')]
		ifFalse: [ (RBCondition directlyDefinesInstanceVariable: variableName in: class) ])
		& (RBCondition canUnderstand: self getterMethod in: self definingClass)
		& (RBCondition canUnderstand: self setterMethod in: self definingClass)
		
]

{ #category : #executing }
RBRemoveDirectAccessToVariableTransformation >> privateTransform [
	
	self replaceInstanceAccesses.
	isClassVariable ifTrue: [ self replaceClassAccesses ].
]

{ #category : #private }
RBRemoveDirectAccessToVariableTransformation >> receiver [

	^ receiver ifNil: [ self receiver: 'self'. receiver ]
]

{ #category : #private }
RBRemoveDirectAccessToVariableTransformation >> receiver: aString [

	receiver := aString
]

{ #category : #private }
RBRemoveDirectAccessToVariableTransformation >> replaceClassAccesses [
	| replacer |
	"replace accesses in the metaclass"
	replacer := self parseTreeRewriterClass
		variable: variableName
		getter: self getterMethod
		setter: self setterMethod
		receiver: self receiver.
	self model
		convertClasses: self definingClass theMetaClass withAllSubclasses
		select: [ :aClass | 
			(aClass whichSelectorsReferToClassVariable: variableName)
				reject: [ :each | 
					aClass == self definingClass theMetaClass
						and: [ each == self getterMethod or: [ each == self setterMethod ] ] ] ]
		using: replacer.

	"replace accessed in the class"
	replacer := self parseTreeRewriterClass
		variable: variableName
		getter: 'class ' , self getterMethod
		setter: 'class ' , self setterMethod.
	self model
		convertClasses: self definingClass theNonMetaClass withAllSubclasses
		select: [ :aClass | aClass whichSelectorsReferToClassVariable: variableName ]
		using: replacer
]

{ #category : #private }
RBRemoveDirectAccessToVariableTransformation >> replaceInstanceAccesses [

	| replacer includeGettersAndSetters |
	includeGettersAndSetters := receiver isNotNil.
	replacer := RBParseTreeRewriter 
				variable: variableName
				getter: self getterMethod
				setter: self setterMethod
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
						and: [ each == self getterMethod
						or: [ each == self setterMethod ] ] ] ] ]
		using: replacer
]

{ #category : #private }
RBRemoveDirectAccessToVariableTransformation >> setterMethod [

	^ setterMethod ifNil: [ 
		setterMethod := (self definingClass setterMethodFor: variableName)
			ifNil: [ 	(variableName uncapitalized, ':') asSymbol ] ]
]
