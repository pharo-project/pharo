"
I'm a command to create a Pull Request
"
Class {
	#name : #IceGitHubNewPullRequestCommand,
	#superclass : #IceGitHubCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #activation }
IceGitHubNewPullRequestCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: IceGitHubMenuGroup
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceGitHubNewPullRequestCommand class >> defaultMenuItemName [
	^ 'Create Pull Request...'
]

{ #category : #execute }
IceGitHubNewPullRequestCommand >> execute [ 

	^ IceGitHubNewPullRequestAction new
		repository: self repository;
		execute.

]
