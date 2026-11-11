"
I am the version source mvs reads real repositories through: I ask the repository Metacello's platform creates for a repository string (Iceberg's when it integrates) for its versions (MCRepository>>versions) and for the baseline at a version (MCRepository>>baselineOf:atVersion:). Fetching, caching and reading files are the repository's.

Repositories that know no versions (Monticello's) answer none, so nothing of them is ever selected.
"
Class {
	#name : 'MetacelloMVSRepositorySource',
	#superclass : 'MetacelloMVSVersionSource',
	#instVars : [
		'repositories'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'reading' }
MetacelloMVSRepositorySource >> baselineOf: aCandidate [
	"The baseline of the version of aCandidate, as its repository reads it. Never signals: what cannot be read becomes a problem"

	| findings definitions |
	findings := MetacelloMVSBaselineDefinitions none.
	aCandidate path ifNotNil: [ :written |
			findings
				warning: #writtenPath
				text: aCandidate repositoryDescription , ' writes the path ' , written , ', an internal detail of the repository' ].
	definitions := [ (self repositoryAt: aCandidate repository) baselineOf: aCandidate project atVersion: aCandidate version string ]
		               on: Error
		               do: [ :error |
				               findings error: #repositoryFailure text: aCandidate repository , ': ' , error description.
				               ^ findings ].
	definitions ifNil: [
			findings error: #missingBaseline text: aCandidate printString , ' has no BaselineOf' , aCandidate project.
			^ findings ].
	^ findings
		  setDefinitions: definitions repository: aCandidate repository , ':' , aCandidate version string;
		  yourself
]

{ #category : 'initialization' }
MetacelloMVSRepositorySource >> initialize [

	super initialize.
	repositories := Dictionary new
]

{ #category : 'reading' }
MetacelloMVSRepositorySource >> repositoryAt: aRepositoryString [
	"The repository Metacello's platform creates for aRepositoryString, once. Not added to Monticello's repository group: resolving registers nothing"

	^ repositories at: aRepositoryString ifAbsentPut: [
			  MetacelloPlatform current createRepository: ((MetacelloProjectSpec project: MetacelloProject new)
					   repository: aRepositoryString;
					   yourself) repositorySpecs first ]
]

{ #category : 'reading' }
MetacelloMVSRepositorySource >> versionsOf: aRepository ifFail: aBlock [
	"The repository is not ours: whatever it fails with (unknown host, no network) is the value of aBlock with why"

	^ [ (self repositoryAt: aRepository) versions asArray ]
		  on: Error
		  do: [ :error | aBlock value: aRepository , ': ' , error description ]
]
