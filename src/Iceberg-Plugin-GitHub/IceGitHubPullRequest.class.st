"
I'm a pull request from github API. 
I represent (more or less), what is described here: https://developer.github.com/v3/pulls/#list-pull-requests
"
Class {
	#name : #IceGitHubPullRequest,
	#superclass : #Object,
	#instVars : [
		'repository',
		'json',
		'fetched',
		'branch',
		'detail',
		'status'
	],
	#category : #'Iceberg-Plugin-GitHub-Model'
}

{ #category : #'instance creation' }
IceGitHubPullRequest class >> fromJSON: aDictionary [
	^ self basicNew
		initializeWithJSON: aDictionary;
		yourself
]

{ #category : #actions }
IceGitHubPullRequest >> acceptMessage: messageString type: typeString [
	IceGitHubAPI new 
		acceptPullRequest: self owner 
			project: self projectName
			number: self number
			data: { 
				#commit_title -> (messageString lines 
					ifNotEmpty: [ :each | each first trimBoth ] 
					ifEmpty: [ '' ]). 
				#commit_message -> (messageString lines 
					ifNotEmpty: [ :lines | (lines allButFirst joinUsing: String lf) trimBoth ] 
					ifEmpty: [ '' ]). 
				#sha-> self headSHA.
				#merge_method -> typeString } asDictionary
]

{ #category : #actions }
IceGitHubPullRequest >> addComment: messageString [
	IceGitHubAPI new 
		addComment: self owner 
			project: self projectName
			number: self number
			data: { #body -> messageString } asDictionary
]

{ #category : #accessing }
IceGitHubPullRequest >> author [
	^ (self jsonAt: 'user') at: 'login'
]

{ #category : #accessing }
IceGitHubPullRequest >> baseLabel [
	^ (self jsonAt: #base) at: #label
]

{ #category : #accessing }
IceGitHubPullRequest >> baseSHA [
	^ (self jsonAt: #base) at: #sha
]

{ #category : #accessing }
IceGitHubPullRequest >> body [
	^ self jsonAt: #body
]

{ #category : #accessing }
IceGitHubPullRequest >> branch [
	fetched ifFalse: [ self fetch ].
	^ branch ifNil: [
		branch := (self repository
			remoteNamed: self remote name )
				remoteBranchNamed: ((self jsonAt: #head) at: #ref) ]
]

{ #category : #accessing }
IceGitHubPullRequest >> branchName [
	^ self branch name
]

{ #category : #accessing }
IceGitHubPullRequest >> commits [
	^ self branch commits
]

{ #category : #accessing }
IceGitHubPullRequest >> detail [
	^ detail ifNil: [
		detail := IceGitHubAPI new get: ((self jsonAt: #url) allButFirst: 23) ]
]

{ #category : #actions }
IceGitHubPullRequest >> fetch [
	[ ^ self remote fetch ]
	ensure: [ fetched := true ]
]

{ #category : #accessing }
IceGitHubPullRequest >> headLabel [
	^ (self jsonAt: #head) at: #label
]

{ #category : #accessing }
IceGitHubPullRequest >> headSHA [
	^ (self jsonAt: #head) at: #sha
]

{ #category : #initialization }
IceGitHubPullRequest >> initialize [
	super initialize.
	fetched := false
]

{ #category : #initialization }
IceGitHubPullRequest >> initializeWithJSON: aDictionary [
	json := aDictionary.
	self initialize
]

{ #category : #'private accessing' }
IceGitHubPullRequest >> json [
	^ json
]

{ #category : #'private accessing' }
IceGitHubPullRequest >> jsonAt: aKey [
	^ self json at: aKey
]

{ #category : #actions }
IceGitHubPullRequest >> mergeIntoWorkingCopy [
	self repository 
		ensureBranch: self branchName;
		checkoutBranch: self branchName.
	self branch merge
]

{ #category : #accessing }
IceGitHubPullRequest >> number [
	^ self jsonAt: #number
]

{ #category : #accessing }
IceGitHubPullRequest >> owner [ 
	^ (((self jsonAt: #base) at: #repo) at: #owner) at: #login
]

{ #category : #accessing }
IceGitHubPullRequest >> projectName [
	^ ((self jsonAt: #base) at: #repo) at: #name
]

{ #category : #actions }
IceGitHubPullRequest >> rejectMessage: messageString [
	messageString trimBoth 
		ifNotEmpty: [ self addComment: messageString ].
	IceGitHubAPI new 
		updatePullRequest: self owner 
			project: self projectName
			number: self number
			data: { #state -> #closed } asDictionary
]

{ #category : #accessing }
IceGitHubPullRequest >> remote [
	| remote |
	remote := IceGitRemote 
		name: self author
		url: (((self jsonAt: #head) at: #repo) at: #ssh_url).
	^ self repository remotes 
		detect: [ :each | each = remote ]
		ifFound: [ :each | each ]
		ifNone: [ 
			self repository addRemote: remote.
			remote ].
	
	
]

{ #category : #accessing }
IceGitHubPullRequest >> repository [
	^ repository
]

{ #category : #accessing }
IceGitHubPullRequest >> repository: anObject [
	repository := anObject
]

{ #category : #accessing }
IceGitHubPullRequest >> state [
	^ self jsonAt: #state
]

{ #category : #accessing }
IceGitHubPullRequest >> status [
	^ status ifNil: [ 
		status := IceGitHubAPI new 
			get: 'repos/', self owner, '/', self projectName, '/commits/', self headSHA, '/status' ]
]

{ #category : #accessing }
IceGitHubPullRequest >> title [
	^ self jsonAt: #title
]

{ #category : #accessing }
IceGitHubPullRequest >> url [
	^ self jsonAt: #url
]
