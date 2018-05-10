"
I'm the abstract superclass of all the code declarations that should exist in an exported file.
"
Class {
	#name : #CodeChunk,
	#superclass : #Object,
	#instVars : [
		'contents'
	],
	#category : #'CodeImport-Chunks'
}

{ #category : #'instance creation' }
CodeChunk class >> contents: someContents [
	^self new
			contents: someContents;
			yourself
]

{ #category : #visiting }
CodeChunk >> accept: aVisitor [

	^ aVisitor visitCodeChunk: self
]

{ #category : #accessing }
CodeChunk >> contents [
	^ contents
]

{ #category : #accessing }
CodeChunk >> contents: anObject [
	contents := anObject
]

{ #category : #importing }
CodeChunk >> import [
	self importFor: nil logSource: true
]

{ #category : #importing }
CodeChunk >> importFor: aRequestor logSource: logSource [
	self import
]

{ #category : #testing }
CodeChunk >> isClassCommentDeclaration [

	^ false
]

{ #category : #testing }
CodeChunk >> isClassOrganizationDeclaration [

	^ false
]

{ #category : #testing }
CodeChunk >> isDoItDeclaration [

	^ false
]

{ #category : #testing }
CodeChunk >> isMethodDeclaration [

	^ false
]

{ #category : #printing }
CodeChunk >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: contents.
	aStream nextPut: $).
]
