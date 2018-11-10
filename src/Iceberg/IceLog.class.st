"
I'm an abstract log tool.
I'm used to crate a full history of a method (see #historyOfMethod:)

I'm used through a repository: 

aRepository ice historyOfMethod: aMethod
"
Class {
	#name : #IceLog,
	#superclass : #Object,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceLog class >> for: aRepository [
	^ self basicNew
		initializeRepository: aRepository;
		yourself	
]

{ #category : #testing }
IceLog class >> isAvailableFor: aRepository [
	^ false
]

{ #category : #'instance creation' }
IceLog class >> new [
	self error: '#for:'
]

{ #category : #private }
IceLog >> basepathOf: aPath [
	self subclassResponsibility
]

{ #category : #private }
IceLog >> collectCommitsFor: aMethod path: aPath [
	| commit pathSpec options commits |

	commit := self repository headCommit.
	pathSpec := LGitPathSpec withAll: { aPath }.
	options := LGitDiffOptions defaults.
	options pathspec: (LGitStringArray withAll: { aPath }).

	commits := OrderedCollection new.
	self repository newCommitWalk 
		fromCommit: commit;
		rawResultsDo: [ :eachCommit | | parents tree | 
			parents := eachCommit numberOfParents.
			tree := eachCommit tree.
			parents = 0 
				ifTrue: [
					(tree matchesPathSpec: pathSpec)
						ifTrue: [ commits add: eachCommit ] ]
				ifFalse: [ 
					eachCommit parents do: [ :eachParent | | diff |
						diff := tree diffTo: eachParent tree options: options. 
						diff numberOfDeltas > 0 
							ifTrue: [ commits add: eachCommit ] ] ] ].

	^ commits
]

{ #category : #private }
IceLog >> collectVersionsFrom: aCollection method: aMethod path: aPath [
	| lastDefinition basepath history |
		
	basepath := self basepathOf: aPath.
	lastDefinition := MCMethodDefinition new.
	history := OrderedCollection new.
	aCollection reverseDo: [ :eachCommit | | definition |
		definition := self definitionFor: aMethod path: basepath commit: eachCommit.
		lastDefinition = definition  ifFalse: [ 
			history add: (IceLogVersion 
				commit: (self repository commitFromGitCommit: eachCommit)
				definition: definition asRingDefinition).
			lastDefinition := definition ] ].

	^ history reversed
]

{ #category : #private }
IceLog >> definitionFor: aMethod path: aPath commit: aCommit [
	self subclassResponsibility
]

{ #category : #accessing }
IceLog >> historyOfMethod: aMethod [
	| path commits |

	path := self toFileName: aMethod.
	commits := self collectCommitsFor: aMethod path: path.
	
	^ self 
		collectVersionsFrom: commits 
		method: aMethod 
		path: path
]

{ #category : #private }
IceLog >> iceCommitFrom: aLGitCommit [
	^ self repository iceCommitFrom: aLGitCommit
]

{ #category : #initialization }
IceLog >> initializeRepository: aRepository [
	repository := aRepository.
	self initialize
]

{ #category : #accessing }
IceLog >> repository [
	^ repository
]

{ #category : #private }
IceLog >> toFileName: aMethod [
	^ self subclassResponsibility
]
