"
Description
--------------------

I am a command opening the repository page of the remote on GitHub if possible.
"
Class {
	#name : #IceGitHubOpenOnGithubCommand,
	#superclass : #IceGitHubBranchSelectCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #activation }
IceGitHubOpenOnGithubCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: IceGitHubMenuGroup
		for: IceTipRepositoryListContext
]

{ #category : #execute }
IceGitHubOpenOnGithubCommand >> execute [
	(IceGitHubOpenOnGithubAction organisation: self remote owner projectName: self remote projectName) execute
]

{ #category : #accessing }
IceGitHubOpenOnGithubCommand >> itemNameWithoutBranches [
	^ 'Open on github'
]
