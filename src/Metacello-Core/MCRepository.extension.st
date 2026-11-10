Extension { #name : 'MCRepository' }

{ #category : '*Metacello-Core' }
MCRepository >> asRepositorySpec [

	self subclassResponsibility
]

{ #category : '*Metacello-Core' }
MCRepository >> baselineOf: aProjectName atVersion: aVersionName [
	"The Monticello definitions of BaselineOf<aProjectName> at the version named aVersionName (one of my versions), read without checking out or installing anything; nil when there is none. None by default: I answer no versions"

	^ nil
]

{ #category : '*Metacello-Core' }
MCRepository class >> createRepositoryFromSpec: aRepositorySpec [
	self subclassResponsibility
]

{ #category : '*Metacello-Core' }
MCRepository class >> findFiletreeAlternateFormat: aRepositorySpec ifFound: aBlock [
	| repositoryClass |
	repositoryClass := self allSubclasses detect: [ :each | each isFiletreeAlternateFormat: aRepositorySpec ] ifNone: [ nil ].
	
	^ repositoryClass ifNotNil: [ aBlock cull: repositoryClass ]
]

{ #category : '*Metacello-Core' }
MCRepository >> flushForScriptGet [
    "noop"
]

{ #category : '*Metacello-Core' }
MCRepository class >> isAvailableFor: type [
	^ false
]

{ #category : '*Metacello-Core' }
MCRepository class >> isFiletreeAlternateFormat: aRepositorySpec [
	^ false
]

{ #category : '*Metacello-Core' }
MCRepository class >> newRepositoryFromSpec: aRepositorySpec [

	^ self allSubclasses
		  detect: [ :each | each isAvailableFor: aRepositorySpec type ]
		  ifFound: [ :repositoryClass | repositoryClass createRepositoryFromSpec: aRepositorySpec ]
		  ifNone: [ self error: 'Unknown repository type: ' , aRepositorySpec description ]
]

{ #category : '*Metacello-Core' }
MCRepository >> repositoryBranchName [
  "extract a branch name from the repository ... if possible"

  ^ ''
]

{ #category : '*Metacello-Core' }
MCRepository >> repositoryDescription [
  "return a description that includes a repositoryVersionString and repositoryBranchName if present"

  ^ self description
]

{ #category : '*Metacello-Core' }
MCRepository >> repositoryVersionString [
  "extract a version string from the repository ... if possible"

  ^ ''
]

{ #category : '*Metacello-Core' }
MCRepository >> versions [
	"The versions of the project I hold, as I publish them (each answers name and date, like a git tag). None by default: only repositories that know their versions, such as Iceberg's, answer some"

	^ #(  )
]
