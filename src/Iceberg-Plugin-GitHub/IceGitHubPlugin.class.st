"
I'm a pluging to handle GitHub issues integration. 
It just add options to easily handle the creation of branchs with the ""convention"" issue naming format (number-plus-normalised-title): 

123-a-sample-of-issue-branch

"
Class {
	#name : #IceGitHubPlugin,
	#superclass : #IcePlugin,
	#category : #'Iceberg-Plugin-GitHub-Core'
}

{ #category : #testing }
IceGitHubPlugin class >> isAvailableFor: aRepository [

	^ aRepository remotes anySatisfy: [ :each | each host = 'github.com' ]
]

{ #category : #'accessing-ui' }
IceGitHubPlugin >> checkoutBranchPanelsOnModel: aRepositoryModel [ 
	
	^ { IceGitHubNewBranchFromIssuePanel on: aRepositoryModel }
]
