Class {
	#name : #IceUnbornRepositoryProperties,
	#superclass : #IceRepositoryProperties,
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #testing }
IceUnbornRepositoryProperties >> addToChangeBuilder: anIceChangeImporter [ 

	"I'm unborn"
	^ self
]

{ #category : #testing }
IceUnbornRepositoryProperties >> asBornProperties [
	
	^ IceRepositoryProperties
		fromDictionary: properties
		commitish: commitish
]

{ #category : #testing }
IceUnbornRepositoryProperties >> isUnborn [

	^ true
]
