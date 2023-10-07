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
	version := MetacelloPlatform current newVersionForWorkingCopy: aWorkingCopy.
	self gofer repositories
		do: [ :repository | repository storeVersion: version ]
]
