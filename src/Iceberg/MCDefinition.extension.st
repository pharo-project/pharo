Extension { #name : #MCDefinition }

{ #category : #'*Iceberg-Changes' }
MCDefinition >> addAnalysisToPackageChangeSet: aPackageChangeSet change: aChange [
	aPackageChangeSet addChange: aChange
]

{ #category : #'*Iceberg-Changes' }
MCDefinition >> addToPackageDiff: anIcePackageDiff leftSnapshot: anObject rightSnapshot: anObject1 [ 
	
	self subclassResponsibility
]

{ #category : #'*Iceberg-Changes' }
MCDefinition >> buildOn: anIceDiffBuilder [
	
	self subclassResponsibility
]

{ #category : #'*Iceberg-changes' }
MCDefinition >> isFileDefinition [
	
	^ false
]
