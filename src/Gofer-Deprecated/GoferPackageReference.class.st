"
A GoferPackageReference refers to the latest version of a Monticello package.
"
Class {
	#name : 'GoferPackageReference',
	#superclass : 'GoferReference',
	#category : 'Gofer-Deprecated-References',
	#package : 'Gofer-Deprecated',
	#tag : 'References'
}

{ #category : 'private' }
GoferPackageReference >> matches: aResolvedReference [

	^ self packageName = aResolvedReference packageName
]

{ #category : 'accessing' }
GoferPackageReference >> packageName [

	^ name
]
