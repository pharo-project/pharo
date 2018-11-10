Extension { #name : #MCVersion }

{ #category : #'*Iceberg-Adapters' }
MCVersion >> directoryName [
	^ (MCFileTreeRepository parseName: self info name) first , '.package'
]
