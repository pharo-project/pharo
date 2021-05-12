"
I am the glonal environment. The default is Smalltalk globals
"
Class {
	#name : #OCEnvironmentScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'environment'
	],
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #'instance creation' }
OCEnvironmentScope class >> for: anEnvironment [  
	^self new environment: anEnvironment
]

{ #category : #accessing }
OCEnvironmentScope >> environment [
	^environment
]

{ #category : #accessing }
OCEnvironmentScope >> environment: aCollection [ 
	environment := aCollection
]

{ #category : #lookup }
OCEnvironmentScope >> hasBindingThatBeginsWith: aString [
	"check weather there are any bindings defined in the environment that start with aString"

	^environment hasBindingThatBeginsWith: aString

]
