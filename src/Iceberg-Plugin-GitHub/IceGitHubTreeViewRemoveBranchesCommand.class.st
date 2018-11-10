"
I'm a command to remove branches from remotes.
"
Class {
	#name : #IceGitHubTreeViewRemoveBranchesCommand,
	#superclass : #IceGitHubTreeCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceGitHubTreeViewRemoveBranchesCommand >> defaultMenuItemName [
	^ 'Remove Remote Branches'
]

{ #category : #accessing }
IceGitHubTreeViewRemoveBranchesCommand >> execute [
	^ IceGitHubRemoveBranchesAction new
		repository: self repository;
		remote: self remote;
		execute.

]
