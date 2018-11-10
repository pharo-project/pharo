Extension { #name : #MCMethodDefinition }

{ #category : #'*Iceberg-Changes' }
MCMethodDefinition >> buildOn: anIcePackageDiff [ 
	self shouldBeImplemented.
]

{ #category : #'*Iceberg-Adapters' }
MCMethodDefinition >> classCategory [
	^ self actualClass ifNotNil: #category ifNil: [ RPackage defaultPackageName ]

]

{ #category : #'*Iceberg-Changes' }
MCMethodDefinition >> method [
	^ self actualClass >> self selector
]
