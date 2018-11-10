"
I'm a presenter to define a new remote repository from a github repository (owner/project).

"
Class {
	#name : #IceTipGitHubRepositoryPanel,
	#superclass : #IceTipGitProviderRepositoryPanel,
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #accessing }
IceTipGitHubRepositoryPanel class >> icon [
	^ (self iconNamed: #github) lighter: 0.50 "just to be sure is visible in all themes"
]

{ #category : #accessing }
IceTipGitHubRepositoryPanel class >> order [
	^ 100
]

{ #category : #accessing }
IceTipGitHubRepositoryPanel class >> providerName [
	^ 'GitHub'
]

{ #category : #specs }
IceTipGitHubRepositoryPanel class >> title [
	^ 'Clone From github.com'
]

{ #category : #accessing }
IceTipGitHubRepositoryPanel >> configureBuilder: aBuilder [

	aBuilder beGithub
]

{ #category : #'querying-github' }
IceTipGitHubRepositoryPanel >> getGitHubRepository [
	[ ^ IceGitHubAPI new
		beAnonymous;
		getRepository: self userName project: self projectName ]
		on: LGitNoCredentialsProvided
		do: [ ^ nil ]
]

{ #category : #accessing }
IceTipGitHubRepositoryPanel >> newRepository [

	|  newRepository |
	newRepository := super newRepository.
	UIManager default 
		informUser: 'Configuring Remotes...'
		during: [ | githubRepository |
			githubRepository := self getGitHubRepository.
			githubRepository ifNil: [ ^ newRepository ].
			(githubRepository at: #fork)
				ifTrue: [ | parent remote |
					parent := githubRepository at: #parent.
					remote := IceGitRemote name: ((parent at: #owner) at: #login) url: (parent at: self selectedProtocol githubRequestUrlKey).
					newRepository addRemote: remote.
					remote fetch. ] ].
	^ newRepository
]
