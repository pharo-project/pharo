Extension { #name : 'MCRepository' }

{ #category : '*Metacello-Core' }
MCRepository >> asRepositorySpecFor: aMetacelloMCProject [

	self subclassResponsibility
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
MCRepository >> metacelloProjectClassFor: aScriptEngine [
    ^ MetacelloProject
]

{ #category : '*Metacello-Core' }
MCRepository class >> newRepositoryFromSpec: aRepositorySpec [

	^ self allSubclasses
		  detect: [ :each | each isAvailableFor: aRepositorySpec type ]
		  ifFound: [ :repositoryClass | repositoryClass createRepositoryFromSpec: aRepositorySpec ]
		  ifNone: [ self error: 'Unknown repository type: ' , aRepositorySpec description ]
]

{ #category : '*Metacello-Core' }
MCRepository >> projectVersion: aString [
    "noop"
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
