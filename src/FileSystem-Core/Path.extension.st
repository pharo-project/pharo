Extension { #name : #Path }

{ #category : #'*FileSystem-Core' }
Path >> asFileReference [
	^ FileSystem disk referenceTo: self
]

{ #category : #'*FileSystem-Core' }
Path >> relativeToReference: aReference [ 
	^ self relativeToPath: aReference path
]
