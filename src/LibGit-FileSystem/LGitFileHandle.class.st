Class {
	#name : #LGitFileHandle,
	#superclass : #FileSystemHandle,
	#instVars : [
		'entry'
	],
	#category : #'LibGit-FileSystem'
}

{ #category : #public }
LGitFileHandle >> binaryReadStream [
	^ self readStream
]

{ #category : #public }
LGitFileHandle >> open [

	entry := self basicOpen
]

{ #category : #public }
LGitFileHandle >> readStream [

	^ ZnBufferedReadStream on: (LGitReadStream on: entry object).
]
