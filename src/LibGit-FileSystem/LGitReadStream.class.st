Class {
	#name : #LGitReadStream,
	#superclass : #Object,
	#instVars : [
		'rawBuffer',
		'gitBlob',
		'position'
	],
	#category : #'LibGit-FileSystem'
}

{ #category : #'instance creation' }
LGitReadStream class >> on: aBlob [
	
	^ self new
		gitBlob: aBlob;
		yourself
]

{ #category : #accessing }
LGitReadStream >> atEnd [
	
	^ position = self size
]

{ #category : #'initialize-release' }
LGitReadStream >> close [
	
	"Do nothing, we should not free git's blob"
]

{ #category : #accessing }
LGitReadStream >> gitBlob: aBlob [
	gitBlob := aBlob
]

{ #category : #initialization }
LGitReadStream >> initialize [
	super initialize.
	position := 0
]

{ #category : #testing }
LGitReadStream >> isBinary [
	"We always give binary data from git.
	It is the user's reponsibility to decode it"
	^ true
]

{ #category : #accessing }
LGitReadStream >> readInto: aBuffer startingAt: start count: bytesToRead [
	| limit |
	limit := bytesToRead min: (self size - position).
	LibC
		memCopy: gitBlob rawBuffer + position
		to: aBuffer
		size: limit.
	position := position + limit.
	^ limit
]

{ #category : #accessing }
LGitReadStream >> size [

	^ gitBlob rawSize
]
