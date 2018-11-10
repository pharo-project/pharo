Class {
	#name : #LGitFunctionReturn,
	#superclass : #Object,
	#instVars : [
		'methodSelector',
		'value'
	],
	#category : #'LibGit-Core-FFI-Handling'
}

{ #category : #'instance creation' }
LGitFunctionReturn class >> methodSelector: aSymbol value: anObject [
	^ self new
		methodSelector: aSymbol;
		value: anObject;
		yourself
]

{ #category : #handling }
LGitFunctionReturn >> handleLGitReturnCode [
	^ self value handleLGitReturnCode
]

{ #category : #accessing }
LGitFunctionReturn >> methodSelector [
	^ methodSelector
]

{ #category : #accessing }
LGitFunctionReturn >> methodSelector: aSymbol [
	methodSelector := aSymbol
]

{ #category : #accessing }
LGitFunctionReturn >> value [
	^ value
]

{ #category : #accessing }
LGitFunctionReturn >> value: anObject [
	value := anObject
]
