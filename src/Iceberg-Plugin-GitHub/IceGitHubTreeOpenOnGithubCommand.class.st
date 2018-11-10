"
Description
--------------------

I am a command opening the repository page of the remote on GitHub if possible.
"
Class {
	#name : #IceGitHubTreeOpenOnGithubCommand,
	#superclass : #IceGitHubTreeCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceGitHubTreeOpenOnGithubCommand >> defaultMenuItemName [
	^ 'Open on github'
]

{ #category : #execute }
IceGitHubTreeOpenOnGithubCommand >> execute [
	(IceGitHubOpenOnGithubAction organisation: self remote owner projectName: self remote projectName) execute
]
