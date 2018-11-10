"
I'm an action to view the open pull Requests.
"
Class {
	#name : #IceGitHubViewPullRequestAction,
	#superclass : #IceGitHubAction,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #executing }
IceGitHubViewPullRequestAction >> basicExecute [
	credentials := IceGitHubAPI ensureCredentials.
	
	UIManager default
		informUser: ('Retrieving pull requests from {1}' format: {self remote})
		during: [ (IceTipGitHubPullRequestListBrowser onRepository: self repository)
				remote: self remote;
				refreshPullRequests;
				openWithSpec ]
]
