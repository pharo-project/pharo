Extension { #name : #LGitCommit }

{ #category : #'*Iceberg' }
LGitCommit >> asIcebergObjectInRepository: iceRepository [
	^ iceRepository commitFromGitCommit: self
]

{ #category : #'*Iceberg-Core' }
LGitCommit >> changesFileNamed: fileName [
	| lookup myVersion parentVersions |
	lookup := [:commit | [ commit entryByPath: fileName ] on: Error do: [ nil ]].
	myVersion := lookup value: self.
	parentVersions := self parents collect: lookup.
	
	^ (myVersion 
		ifNil: [ parentVersions includes: nil ]
		ifNotNil: [ parentVersions anySatisfy: [ :version | 
				version notNil and: [ version objectId = myVersion objectId ]]]) not

]
