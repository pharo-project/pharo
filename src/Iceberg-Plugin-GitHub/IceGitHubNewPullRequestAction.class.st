"
I'm an action to create a new pull request.
"
Class {
	#name : #IceGitHubNewPullRequestAction,
	#superclass : #IceGitHubAction,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #private }
IceGitHubNewPullRequestAction >> askRemote: aRepository [

	^ aRepository remotes anyOne
]

{ #category : #executing }
IceGitHubNewPullRequestAction >> basicExecute [

	credentials := IceGitHubAPI ensureCredentials.

	remote ifNil: [ 
		remote := self repository branch hasUpstream
			ifTrue: [ self repository branch upstream remote ]
			ifFalse: [ self askRemote: self repository ]
	].

	(self validateMakePullRequestOn: self repository) ifFalse: [ ^ #() ].

	(IceGitHubCreatePullRequestModel 
		repository: self repository
		credentials: self credentials
		headRemote: remote)
		setModal: true;
		onAccept: [ :pullRequest | 
			[ | pullRequestDatas url |
			pullRequestDatas := pullRequest send.
			url := pullRequestDatas at: 'html_url'.
			UIManager default 
				inform: 'Pull request created. Click to view on Github.' 
				actionOnClick: [ self class environment at: #WebBrowser ifPresent: [ :webBrowser | webBrowser openOn: url ] ifAbsent: [ self inform: ('Cannot open "{1}" because the project WebBrowser is not present by default in Pharo 6.' format: { url }) ]  ] ]
				on: IceGitHubError
				do: [ :e | self reportError: e ] ];
		openWithSpec
]

{ #category : #private }
IceGitHubNewPullRequestAction >> validateMakePullRequestOn: aRepository [
	| status |
	
	status := OrderedCollection new: 2.
	aRepository isModified 
		ifTrue: [ status add: 'Uncommited changes' ].
	(aRepository outgoingCommitsTo: remote) 
		ifNotEmpty: [ :commits | '{1} not published' format: { commits size } ].
	status ifEmpty: [ ^ true ].
	
	^ UIManager default 
		confirm: (('{1} has ongoing modifications ({2}). 
Do you want to continue anyway?') format: { aRepository name. status asCommaString })
]
