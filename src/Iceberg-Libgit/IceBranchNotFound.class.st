Class {
	#name : #IceBranchNotFound,
	#superclass : #IceError,
	#instVars : [
		'branchName'
	],
	#category : #'Iceberg-Libgit-Exceptions'
}

{ #category : #accessing }
IceBranchNotFound >> branchName [
	^ branchName
]

{ #category : #accessing }
IceBranchNotFound >> branchName: aString [ 
	branchName := aString
]
