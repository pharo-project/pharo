Extension { #name : #MCRepository }

{ #category : #'*Metacello-Core' }
MCRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	self subclassResponsibility
]

{ #category : #'*Metacello-Core' }
MCRepository class >> findFiletreeAlternateFormat: aRepositorySpec ifFound: aBlock [
	| repositoryClass |
	repositoryClass := self allSubclasses detect: [ :each | each isFiletreeAlternateFormat: aRepositorySpec ] ifNone: [ nil ].
	
	^ repositoryClass ifNotNil: [ aBlock cull: repositoryClass ]
]

{ #category : #'*metacello-core' }
MCRepository >> flushForScriptGet [
    "noop"
]

{ #category : #'*Metacello-Core' }
MCRepository class >> isAvailableFor: type [
	^ false
]

{ #category : #'*Metacello-Core' }
MCRepository class >> isFiletreeAlternateFormat: aRepositorySpec [
	^ false
	
]

{ #category : #'*Metacello-Core' }
MCRepository class >> newRepositoryFromSpec: aRepositorySpec on: aPlatform [

	^ self allSubclasses
		detect: [ :each | each isAvailableFor: aRepositorySpec type ]
		ifFound: [ :repositoryClass | 
			repositoryClass
				createRepositoryFromSpec: aRepositorySpec
				on: aPlatform ]
		ifNone: [ self error: 'Unknown repository type: ' , aRepositorySpec description ]
]

{ #category : #'*metacello-core' }
MCRepository >> repositoryBranchName [
  "extract a branch name from the repository ... if possible"

  ^ ''
]

{ #category : #'*metacello-core' }
MCRepository >> repositoryDescription [
  "return a description that includes a repositoryVersionString and repositoryBranchName if present"

  ^ self description
]

{ #category : #'*metacello-core' }
MCRepository >> repositoryVersionString [
  "extract a version string from the repository ... if possible"

  ^ ''
]
