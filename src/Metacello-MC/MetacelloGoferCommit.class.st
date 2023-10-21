Class {
	#name : 'MetacelloGoferCommit',
	#superclass : 'GoferCommit',
	#category : 'Metacello-MC-Gofer',
	#package : 'Metacello-MC',
	#tag : 'Gofer'
}

{ #category : 'running' }
MetacelloGoferCommit >> execute: aWorkingCopy [
	| version |
	version := aWorkingCopy newVersionIn: aWorkingCopy repositoryGroup.
	self gofer repositories
		do: [ :repository | repository storeVersion: version ]
]
