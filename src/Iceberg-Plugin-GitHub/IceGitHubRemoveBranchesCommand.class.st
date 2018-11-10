"
I'm a command to remove branches from remotes.
"
Class {
	#name : #IceGitHubRemoveBranchesCommand,
	#superclass : #IceGitHubBranchSelectCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #activation }
IceGitHubRemoveBranchesCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: IceGitHubMenuGroup
		for: IceTipRepositoryListContext
]

{ #category : #execute }
IceGitHubRemoveBranchesCommand >> execute [
	^ IceGitHubRemoveBranchesAction new
		repository: self repository;
		remote: self remote;
		execute.

]

{ #category : #accessing }
IceGitHubRemoveBranchesCommand >> itemNameWithoutBranches [
	^ 'Remove Branches...'
]
