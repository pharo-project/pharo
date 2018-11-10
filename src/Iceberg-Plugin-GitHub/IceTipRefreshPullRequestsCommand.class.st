"
Description
--------------------

I am a simple command to refresh the content of the github pull requests browser
"
Class {
	#name : #IceTipRefreshPullRequestsCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'tool'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceTipRefreshPullRequestsCommand class >> defaultHelp [
	^ 'Refresh the pull requests availables'
]

{ #category : #accessing }
IceTipRefreshPullRequestsCommand class >> defaultMenuIconName [
	^ #glamorousRefresh
]

{ #category : #accessing }
IceTipRefreshPullRequestsCommand class >> defaultMenuItemName [
	^ 'Refresh'
]

{ #category : #accessing }
IceTipRefreshPullRequestsCommand class >> defaultPosition [
	^ #right
]

{ #category : #activation }
IceTipRefreshPullRequestsCommand class >> toolbarCommitActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipPullRequestsListContext
]

{ #category : #execution }
IceTipRefreshPullRequestsCommand >> execute [
	UIManager default
		informUser: 'Refreshing pull request list'
		during: [ tool refreshPullRequests ]
]

{ #category : #execution }
IceTipRefreshPullRequestsCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	tool := aToolContext tool
]
