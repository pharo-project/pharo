Extension { #name : 'MetacelloSemanticVersionNumber' }

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> baselineOf: aCandidate from: aSource [
	"The Monticello definitions of the baseline of aCandidate at me, from the repository aSource keeps for its project"

	^ (aSource repositoryAt: aCandidate repository) baselineOf: aCandidate project atVersion: self string
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> descriptionOf: aCandidate [
	"The Metacello repository description to load aCandidate at me: its repository at me. No path: Iceberg reads where the code is from the project"

	^ aCandidate repository , ':' , self string
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> fileFormatOf: aCandidate from: aSource [
	"The format of the packages of aCandidate at me, as the repository aSource keeps for its project tells"

	^ (aSource repositoryAt: aCandidate repository) fileFormatAt: self string
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> hasMajor: anInteger [

	^ self major = anInteger
]

{ #category : '*Metacello-MVS' }
MetacelloSemanticVersionNumber >> isSameMajorAs: aVersion [
	"Whether aVersion, of the same repository, can replace me or be replaced by me: the same major"

	^ aVersion hasMajor: self major
]
