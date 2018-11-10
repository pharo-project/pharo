"
I'm a pluging to handle fogbugz issues integration. 
It just add options to easily handle the creation of branchs with the ""convention"" issue naming format (number-plus-normalised-title): 

12345-a-sample-of-issue-branch

NOTE: In the future we can add other fogbugz elements (like list issues, etc.), but since API is very slow we will consider it very carefully. 
"
Class {
	#name : #IcePharoManuscriptPlugin,
	#superclass : #IcePlugin,
	#category : #'Iceberg-Plugin-Pharo-Manuscript'
}

{ #category : #testing }
IcePharoManuscriptPlugin class >> isAvailableFor: aRepository [

	^ aRepository remotes anySatisfy: [ :each | each projectName = 'pharo' ]
]

{ #category : #'acccessing-ui' }
IcePharoManuscriptPlugin >> checkoutBranchPanelsOnModel: aRepositoryModel [ 
	
	^ { IcePharoNewBranchFromIssuePanel on: aRepositoryModel }
]

{ #category : #testing }
IcePharoManuscriptPlugin >> hasPharoInOrigin: aRepository [

	| origin |
	origin := aRepository origin.
	^ (origin host = 'github.com')
		and: [ origin owner = 'pharo-project'
		and: [ origin projectName = 'pharo' ] ]
]

{ #category : #testing }
IcePharoManuscriptPlugin >> hasPharoInRemotes: aRepository [
	^ aRepository remotes 
		anySatisfy: [ :aRemote | self pharoURLs includes: aRemote url ].
]

{ #category : #testing }
IcePharoManuscriptPlugin >> isAvailableFor: aRepository [

	^ (self hasPharoInOrigin: aRepository) or: [ self hasPharoInRemotes: aRepository ]
]
