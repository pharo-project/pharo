"
I am a remote tracked branch. I am a copy to a branch that exists in a remote. I can thus only synchronize myself with the external repository through fetches and with the local branches.

  branch fetch.
  branch fetchFrom:.

  branch outgoingCommits.
  branch hasOutgoingCommits.
"
Class {
	#name : #IceRemoteBranch,
	#superclass : #IceBranch,
	#category : #'Iceberg-Core'
}

{ #category : #comparing }
IceRemoteBranch >> = aBranch [

	^ aBranch isRemote
		and: [ self name = aBranch name ]
]

{ #category : #'API-checkout' }
IceRemoteBranch >> checkout: aCheckoutStrategy [

	| localBranch |
	self repository 
		branchNamed: self shortName
		ifPresent: [ :branch | IceError signal: ('Branch {1} already exists' format: { self shortName }) ].
	
	localBranch := self repository
		createBranch: self shortName
		inCommit: self commit.

	aCheckoutStrategy committish: localBranch.
	^ localBranch checkout: aCheckoutStrategy
]

{ #category : #'API-actions' }
IceRemoteBranch >> checkoutWithStrategy: aCheckoutStrategy [
	"checkouts a remote branch"

	"if there is a local branch with that name, checkout it (it will be that what you want, you will need to do 
	a pull later)"	
	self repository localBranches
		detect: [ :each | each name = self shortName ]
		ifFound: [ :each | ^ each checkout ].
	"there are no local repositories. First we need to create one."
	self repository
		createBranch: self shortName 
		inCommit: self commit.
	"then we can do the checkout"
	^ super checkoutWithStrategy: aCheckoutStrategy
]

{ #category : #comparing }
IceRemoteBranch >> hash [

	^ self name hash
]

{ #category : #'API-remotes' }
IceRemoteBranch >> incomingCommits [
	^ #()
]

{ #category : #testing }
IceRemoteBranch >> isRemote [

	^ true
]

{ #category : #'API-remotes' }
IceRemoteBranch >> outgoingCommits [
	^ #()
]

{ #category : #accessing }
IceRemoteBranch >> remote [
	^ repository remoteNamed: self remoteName
]

{ #category : #'as yet unclassified' }
IceRemoteBranch >> shortName [
	^ self name copyAfter: $/
]
