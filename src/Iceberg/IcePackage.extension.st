Extension { #name : #IcePackage }

{ #category : #'*Iceberg-Adapters' }
IcePackage >> directoryName [

	^ self repository directoryNameForPackage: self
]
