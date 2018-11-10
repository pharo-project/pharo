Class {
	#name : #LGitExternalObject,
	#superclass : #FFIExternalObject,
	#traits : 'TLGitPrintingTrait + TLGitCalloutTrait',
	#classTraits : 'TLGitPrintingTrait classTrait + TLGitCalloutTrait classTrait',
	#classVars : [
		'LibraryHandle',
		'Session'
	],
	#category : #'LibGit-Core-Abstracts'
}

{ #category : #finalization }
LGitExternalObject class >> finalizeResourceData: aHandle [
	"Call the class specific free frunction if aHandle is still a valid external address."
	aHandle isNull ifTrue: [ ^self ].
	self perform: self freeFunctionSelector with: aHandle.
	aHandle beNull
]

{ #category : #'libgit-finalization' }
LGitExternalObject class >> freeFunctionSelector [
	^ #object_free:
]

{ #category : #'as yet unclassified' }
LGitExternalObject class >> fromHandle: aHandle [
	^self basicNew setHandle: aHandle
]

{ #category : #'libgit-calls' }
LGitExternalObject class >> object_free: handle [ 
	"We pass a handle in here instead of the object. We can't pass the object since that
	has already been gc'ed."
	^self call: #(void git_object_free(void *handle)) options: #(  )
]

{ #category : #'initialize-release' }
LGitExternalObject >> free [
	handle isExternalAddress 
		ifTrue: [ 
			self class finalizeResourceData: handle.
			self assert: handle isNull ]
]

{ #category : #'as yet unclassified' }
LGitExternalObject >> initialize [
	super initialize.
	self autoRelease.
]

{ #category : #testing }
LGitExternalObject >> isReady [
	^ self isNull not
]

{ #category : #printing }
LGitExternalObject >> shouldContinuePrintingOn: aStream [
	self ifNotReady: [ aStream nextPutAll: ' (<not initialized>)' ].
	
	^ self isReady
]
