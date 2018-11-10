"
I represent a change coming from git (in contrast to a change coming from the image). I know the changed file and I can be visited to define the create a more detailed diff.
"
Class {
	#name : #IceGitChange,
	#superclass : #Object,
	#instVars : [
		'filePathString'
	],
	#category : #'Iceberg-Libgit-Changes'
}

{ #category : #accessing }
IceGitChange class >> on: aString [ 
	
	^ self new
		filePathString: aString;
		yourself
]

{ #category : #visiting }
IceGitChange >> accept: aVisitor [
	
	^ aVisitor visitGitChange: self
]

{ #category : #ccessing }
IceGitChange >> child [
	
	^ IceGitChange on: (RelativePath withAll: self path segments allButFirst) pathString
]

{ #category : #displaying }
IceGitChange >> displayingProgressString [
	^ 'Comparing changes of ' , filePathString
]

{ #category : #accessing }
IceGitChange >> filePathString: aString [ 
	filePathString := aString
]

{ #category : #accessing }
IceGitChange >> path [
	
	^ Path from: filePathString
]

{ #category : #printing }
IceGitChange >> printOn: aStream [

	aStream
		nextPutAll: 'gitChange(';
		nextPutAll: filePathString asString;
		nextPutAll: ')'
]
