Extension { #name : #Path }

{ #category : #'*FileSystem-Core' }
Path >> asFileLocatorOrReference [

	^ (FileLocator fromPath: self)
			ifNil: [ self asFileReference ]
]

{ #category : #'*FileSystem-Core' }
Path >> asFileReference [
	^ FileSystem disk referenceTo: self
]

{ #category : #'*FileSystem-Core' }
Path >> relativeToReference: aReference [ 
	^ self relativeToPath: aReference path
]
