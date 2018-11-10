Class {
	#name : #IceNewBranchFromGitHubIssueCommand,
	#superclass : #IceTipCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #testing }
IceNewBranchFromGitHubIssueCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
			byItemOf: IceGitHubMenuGroup
			order: 20
			for: IceTipRepositoryListContext
]

{ #category : #testing }
IceNewBranchFromGitHubIssueCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext repositoryModel remotes anySatisfy: [ :each | each host = 'github.com' ]
]

{ #category : #testing }
IceNewBranchFromGitHubIssueCommand class >> defaultMenuIconName [
	^ #github
]

{ #category : #testing }
IceNewBranchFromGitHubIssueCommand class >> defaultMenuItemName [
	^ 'Create new branch for issue...'
]

{ #category : #testing }
IceNewBranchFromGitHubIssueCommand class >> defaultPosition [

	^ #right
]

{ #category : #testing }
IceNewBranchFromGitHubIssueCommand class >> defaultToolbarItemName [
	^ 'New issue'
]

{ #category : #execution }
IceNewBranchFromGitHubIssueCommand >> execute [

	(IceTipCheckoutBranchDialog on: self repositoryModel)
		withTypes: (IceGitHubPlugin new checkoutBranchPanelsOnModel: self repositoryModel);
		openDialogWithSpec
]
