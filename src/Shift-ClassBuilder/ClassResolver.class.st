"
I am in charge of resolving the existence of a class
"
Class {
	#name : #ClassResolver,
	#superclass : #Object,
	#category : #'Shift-ClassBuilder'
}

{ #category : #resolving }
ClassResolver >> resolve: aCDClassDefinitionNode [
	^self subclassResponsibility
]

{ #category : #resolving }
ClassResolver >> resolve: aClassName inEnv: anEnvironment [
	(anEnvironment classNamed: aClassName) 
		ifNotNil:[:class|^class]
		ifNil:[^self resolve: aClassName].
]
