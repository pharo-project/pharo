Extension { #name : 'MCRepository' }

{ #category : '*Metacello-Core' }
MCRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
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
MCRepository class >> newRepositoryFromSpec: aRepositorySpec on: aPlatform [

	^ self allSubclasses
		detect: [ :each | each isAvailableFor: aRepositorySpec type ]
		ifFound: [ :repositoryClass | 
			repositoryClass
				createRepositoryFromSpec: aRepositorySpec
				on: aPlatform ]
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
