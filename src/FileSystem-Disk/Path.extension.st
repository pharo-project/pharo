Extension { #name : 'Path' }

{ #category : '*FileSystem-Disk' }
Path >> asFileLocatorOrReference [

	^ FileLocator fromPath: self ifNone: [ self asFileReference ]
]

{ #category : '*FileSystem-Disk' }
Path >> asFileReference [
	^ FileSystem disk referenceTo: self
]
