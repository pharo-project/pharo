Class {
	#name : #MetacelloGoferCommit,
	#superclass : #GoferCommit,
	#category : #Metacello-MC-Gofer
}

{ #category : #running }
MetacelloGoferCommit >> execute: aWorkingCopy [
	| version |
	version := MetacelloPlatform current newVersionForWorkingCopy: aWorkingCopy.
	self gofer repositories
		do: [ :repository | repository storeVersion: version ]
]
