"
Description
--------------------

I am a simple command to open the PR review tool.
"
Class {
	#name : #IceTipGitHubOpenPullRequestBrowserCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'item',
		'tool'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #activation }
IceTipGitHubOpenPullRequestBrowserCommand class >> browserPullRequestActivation [
	<classAnnotation>
	^ CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipGitHubOpenPullRequestBrowserCommand class >> browserPullRequestListContextActivation [
	<classAnnotation>
	^ CmdContextMenuActivation byRootGroupItemOrder: 20 for: IceTipPullRequestsListContext
]

{ #category : #testing }
IceTipGitHubOpenPullRequestBrowserCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext item isNotNil
]

{ #category : #accessing }
IceTipGitHubOpenPullRequestBrowserCommand class >> defaultHelp [
	^ 'Open the Pull Request review tool'
]

{ #category : #accessing }
IceTipGitHubOpenPullRequestBrowserCommand class >> defaultMenuIconName [
	^ #smallFind
]

{ #category : #accessing }
IceTipGitHubOpenPullRequestBrowserCommand class >> defaultMenuItemName [
	^ 'Review pull request'
]

{ #category : #execution }
IceTipGitHubOpenPullRequestBrowserCommand >> execute [
	tool viewPullRequest: item
]

{ #category : #execution }
IceTipGitHubOpenPullRequestBrowserCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	item := aToolContext item.
	tool := aToolContext tool
]
