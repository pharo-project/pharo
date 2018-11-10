Extension { #name : #MCRemoval }

{ #category : #'*Iceberg-TipUI' }
MCRemoval >> canBeBrowsed [
	^ false
]

{ #category : #'*Iceberg-TipUI' }
MCRemoval >> icon [
	^ self iconNamed: #changeRemove
]
