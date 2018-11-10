"
Description
--------------------

I am a nil PR that can be used when the user has no PR to see.
"
Class {
	#name : #IceGitHubNilPullRequest,
	#superclass : #Object,
	#category : #'Iceberg-Plugin-GitHub-Utils'
}

{ #category : #accessing }
IceGitHubNilPullRequest >> author [
	^ 'No PR selected'
]

{ #category : #accessing }
IceGitHubNilPullRequest >> baseLabel [
	^ 'No PR selected'
]

{ #category : #accessing }
IceGitHubNilPullRequest >> body [
	^ ''
]

{ #category : #accessing }
IceGitHubNilPullRequest >> headLabel [
	^ 'No PR selected'
]

{ #category : #accessing }
IceGitHubNilPullRequest >> url [
	^ 'No PR selected'
]
