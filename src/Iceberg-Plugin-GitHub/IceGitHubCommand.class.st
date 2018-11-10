"
I'm a generic command to perform plugin actions for GitHub.
My children define the real behavior, and they need to implement #execute.
"
Class {
	#name : #IceGitHubCommand,
	#superclass : #IceTipCommand,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #testing }
IceGitHubCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext repositoryModel entity remotes anySatisfy: [ :remote | remote url includesSubstring: 'github.com' ]
]

{ #category : #execute }
IceGitHubCommand >> execute [ 
	self subclassResponsibility
]

{ #category : #accessing }
IceGitHubCommand >> repository [
	^ repositoryModel entity
]
