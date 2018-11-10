Class {
	#name : #IceUnbornProject,
	#superclass : #IceAbstractProject,
	#category : #'Iceberg-Project'
}

{ #category : #testing }
IceUnbornProject >> isUnborn [
	
	^ true
]

{ #category : #testing }
IceUnbornProject >> readOnlyVersion [

	^ IceInvalidProject onCommitish: commitish
]

{ #category : #accessing }
IceUnbornProject >> sourceDirectory [

	"Tricky method.
	If we are in an unborn project, it means the user has not configured meta-data on it yet.
	In that case, return the empty source directory"
	^ ''
]
