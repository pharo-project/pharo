"
I am a pull request. 
I can be sent using ==#sendWithCredentials:== protocol.
"
Class {
	#name : #IceGitHubPullRequestDefinition,
	#superclass : #Object,
	#instVars : [
		'credentials',
		'title',
		'baseRemote',
		'headRemote',
		'baseBranchName',
		'headBranchName',
		'body'
	],
	#category : #'Iceberg-Plugin-GitHub-Model'
}

{ #category : #accessing }
IceGitHubPullRequestDefinition >> baseBranchName [
	^ baseBranchName
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> baseBranchName: anObject [
	baseBranchName := anObject
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> baseRemote [
	^ baseRemote
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> baseRemote: anObject [
	baseRemote := anObject
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> body [
	^ body
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> body: anObject [
	body := anObject
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> credentials [
	^ credentials
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> credentials: anObject [
	credentials := anObject
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> headBranchName [
	^ headBranchName
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> headBranchName: anObject [
	headBranchName := anObject
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> headRemote [
	^ headRemote
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> headRemote: anObject [
	headRemote := anObject
]

{ #category : #actions }
IceGitHubPullRequestDefinition >> send [
	| pullRequestData |

	pullRequestData := { 
		'title' -> self title.
		'head' -> (self headRemote owner, ':', self headBranchName).
		'base' -> self baseBranchName.
		'body' -> self body.
	} asDictionary.
	
	^ IceGitHubAPI new 
		credentials: self credentials;
		addPullRequest: self baseRemote owner 
			project: self baseRemote projectBasename 
			data: pullRequestData
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> title [
	^ title
]

{ #category : #accessing }
IceGitHubPullRequestDefinition >> title: anObject [
	title := anObject
]
