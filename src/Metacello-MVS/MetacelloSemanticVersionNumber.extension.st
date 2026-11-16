Extension { #name : 'MetacelloSemanticVersionNumber' }

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> baselineOf: aCandidate from: aSource [
	"The Monticello definitions of the baseline of aCandidate at me, from the repository aSource keeps for its project"

	^ (aSource repositoryAt: aCandidate repository) baselineOf: aCandidate project atVersion: self string
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> descriptionOf: aCandidate [
	"The Metacello repository description to load aCandidate at me: its repository at me, with its path"

	^ String streamContents: [ :stream |
			  stream
				  nextPutAll: aCandidate repository;
				  nextPut: $:;
				  nextPutAll: self string.
			  aCandidate path ifNotNil: [ :aPath |
					  stream
						  nextPut: $/;
						  nextPutAll: aPath ] ]
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> isSameMajorAs: aVersion [
	"Whether aVersion, of the same repository, can replace me or be replaced by me: the same major"

	^ self major = aVersion major
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> readDescriptionOf: aCandidate [
	"Where the baseline of aCandidate at me is read from: its repository at me, without a path, since every path of a repository reads the same baseline"

	^ aCandidate repository , ':' , self string
]
