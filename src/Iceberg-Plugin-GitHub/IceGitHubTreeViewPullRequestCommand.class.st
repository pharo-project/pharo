"
I'm a command to show the pull request review browser.
"
Class {
	#name : #IceGitHubTreeViewPullRequestCommand,
	#superclass : #IceGitHubTreeCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceGitHubTreeViewPullRequestCommand >> defaultMenuItemName [
	^ 'View Pull Requests'
]

{ #category : #execute }
IceGitHubTreeViewPullRequestCommand >> execute [
	^ IceGitHubViewPullRequestAction new
		repository: self repository;
		remote: self remote;
		execute.

]
