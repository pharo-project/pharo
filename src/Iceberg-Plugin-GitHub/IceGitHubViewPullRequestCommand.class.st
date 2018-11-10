"
I'm a command to show the pull request review browser.
"
Class {
	#name : #IceGitHubViewPullRequestCommand,
	#superclass : #IceGitHubBranchSelectCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #activation }
IceGitHubViewPullRequestCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: IceGitHubMenuGroup
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceGitHubViewPullRequestCommand class >> defaultMenuItemName [
	^ 'View Pull Request...'
]

{ #category : #execute }
IceGitHubViewPullRequestCommand >> execute [
	^ IceGitHubViewPullRequestAction new
		repository: self repository;
		remote: self remote;
		execute.

]

{ #category : #accessing }
IceGitHubViewPullRequestCommand >> itemNameWithoutBranches [
	^ 'View Pull Request...'
]
