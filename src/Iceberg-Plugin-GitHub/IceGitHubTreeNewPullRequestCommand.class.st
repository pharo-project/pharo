"
I'm a command to create a Pull Request
"
Class {
	#name : #IceGitHubTreeNewPullRequestCommand,
	#superclass : #IceGitHubTreeCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceGitHubTreeNewPullRequestCommand >> defaultMenuItemName [
	^ 'New Pull Request'
]

{ #category : #execute }
IceGitHubTreeNewPullRequestCommand >> execute [ 

	^ IceGitHubNewPullRequestAction new
		repository: self repository;
		remote: self remote;
		execute.

]
