Class {
	#name : #LGitIllegalArgument,
	#superclass : #LGitAbstractError,
	#instVars : [
		'expectedClass'
	],
	#category : #'LibGit-Core-Exceptions'
}

{ #category : #signalling }
LGitIllegalArgument class >> signalWith: anObject shouldBe: aClass [
	^ self new
		object: anObject;
		expectedClass: aClass;
		signal;
		yourself
]

{ #category : #accessing }
LGitIllegalArgument >> expectedClass [
	^ expectedClass
]

{ #category : #accessing }
LGitIllegalArgument >> expectedClass: anObject [
	expectedClass := anObject
]

{ #category : #accessing }
LGitIllegalArgument >> messageText [
	^ 'libgit2 ffi call expects a(n) {1} but instead received {2}'
			format: {self expectedClass. self object}

]
