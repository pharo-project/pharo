Class {
	#name : #CDClassNameNode,
	#superclass : #CDNode,
	#instVars : [
		'className'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDClassNameNode >> binding [
	| classBinding |
	classBinding := self existingBindingIfAbsent: [LiteralVariable key: className value: nil].
	^OCLiteralVariable new 
		scope: originalNode scope;
		assoc: classBinding
]

{ #category : #accessing }
CDClassNameNode >> className [
	
	^ className
]

{ #category : #accessing }
CDClassNameNode >> className: aString [ 
	
	className := aString
]

{ #category : #accessing }
CDClassNameNode >> existingBindingIfAbsent: aBlock [

	| binding |
	binding := originalNode methodNode compilationContext environment bindingOf: className.
	^binding ifNil: aBlock
]

{ #category : #accessing }
CDClassNameNode >> existingClassIfAbsent: aBlock [

	| binding |
	binding := self existingBindingIfAbsent: aBlock.
	^binding value
]

{ #category : #testing }
CDClassNameNode >> isTemp [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDClassNameNode >> isVariable [
	"Existing class name is a variable 
	to be polymorphic to method literal nodes which encode class reference"
	
	self existingBindingIfAbsent: [ ^false ].
	^true
]

{ #category : #accessing }
CDClassNameNode >> name [
	
	^ className
]

{ #category : #accessing }
CDClassNameNode >> value [
	
	^ className
]
