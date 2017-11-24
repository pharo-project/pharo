"
I am an acceptor in the ""weird"" context when the text entered could be a class definition or a method definition.

For example if a package and a class is selected but the user does not define a method but we new class.
"
Class {
	#name : #ClassOrMethodDefinitionAcceptor,
	#superclass : #AbstractAcceptor,
	#category : #'Nautilus-Acceptors'
}

{ #category : #protocol }
ClassOrMethodDefinitionAcceptor >> accept: source notifying: aController [

	self model selectedClass isMeta
		ifTrue: [ self acceptClassSide: source notifying: aController ]
		ifFalse: [ self acceptInstanceSide: source notifying: aController ]
]

{ #category : #protocol }
ClassOrMethodDefinitionAcceptor >> acceptClassSide: source notifying: aController [
	| isClassDefinition |
	
	isClassDefinition := (' ' split: source) first = self model selectedClass theNonMetaClass name.
	
	isClassDefinition
		ifTrue: [
			self model
				compileAClassFrom: source 
				notifying: aController ]
		ifFalse: [
			self model
				compileAMethodFromCategory: Protocol unclassified
				withSource: source
				notifying: aController ]
]

{ #category : #protocol }
ClassOrMethodDefinitionAcceptor >> acceptInstanceSide: source notifying: aController [
	| isClassDefinition |
	
	isClassDefinition := false.
	
	"Try to parse the source. If it succeed, it means the source represents a method definition. On error, we switch to class definition mode"
	[ RBParser parseMethod: source ]
		on: Notification
		do: [ isClassDefinition := true ]. 

	isClassDefinition
		ifTrue: [
			self model
				compileAClassFrom: source 
				notifying: aController ]
		ifFalse: [
			self model
				compileAMethodFromCategory: Protocol unclassified
				withSource: source
				notifying: aController ]
]
