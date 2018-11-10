"
I represent a reference to an upstream repository, i.e. the repository to which you want to push/pull. Usually I am called a  'remote' but I could also be local. Most frequently each repository has at least one remote named 'origin'.

I am abstract, my subclasses provide access to different kinds of remotes.

Right now I just provide information about the different parts of the url, I could get smarter in the future.

Internal Representation and Key Implementation Points.
    Instance Variables
	url:		<String>
"
Class {
	#name : #IceRemote,
	#superclass : #Object,
	#instVars : [
		'url',
		'projectName',
		'name',
		'localRepository'
	],
	#category : #'Iceberg-Core'
}

{ #category : #comparing }
IceRemote >> = another [
	
	self subclassResponsibility
]

{ #category : #private }
IceRemote >> basicUrl: aString [
	url := aString
]

{ #category : #branches }
IceRemote >> branches [
	
	self subclassResponsibility
]

{ #category : #accesing }
IceRemote >> branchesFrom: aRepository [
	"I return all the branches of my remote from a repository"

	^ aRepository branches
		select: [ :branch | branch isRemote and: [ branch remoteName = self name ] ]
]

{ #category : #accessing }
IceRemote >> detailedInfo [
	^ #()
]

{ #category : #displaying }
IceRemote >> displayString [
	^ '{1}: {2}' format: { 
			self name.
			self url }
]

{ #category : #fetching }
IceRemote >> fetch [
	
	self subclassResponsibility
]

{ #category : #fetching }
IceRemote >> fetchBranch: aRemoteBranch [
	
	self subclassResponsibility
]

{ #category : #testing }
IceRemote >> hasBranches [
	^ self branches isNotEmpty
]

{ #category : #testing }
IceRemote >> hasRemoteBranchNamed: aString [ 
	
	self
		remoteBranchNamed: aString
		ifPresent: [ :branch | ^ true ]
		ifAbsent: [ ^ false ]
]

{ #category : #accessing }
IceRemote >> host [
	^ self subclassResponsibility
]

{ #category : #patches }
IceRemote >> httpsUrl [
	"This method is used to compare exchangeable addresses (https:// and git@). 
	 So it has sense in my children, and is notably used on #isSameUrl: 
	 implementations"
	^ self url
]

{ #category : #testing }
IceRemote >> isSame: aRemote [
	^ (self referencesSameRemoteLocationAs: aRemote)
		and: [ self name = aRemote name ]
]

{ #category : #testing }
IceRemote >> isUndefined [
	^ false
]

{ #category : #accessing }
IceRemote >> localRepository [
	
	^ localRepository
]

{ #category : #accessing }
IceRemote >> localRepository: aRepository [
	
	localRepository := aRepository
]

{ #category : #accessing }
IceRemote >> name [
	^ name
]

{ #category : #accessing }
IceRemote >> name: aString [ 
	
	name := aString
]

{ #category : #private }
IceRemote >> parseUrl [
	^ self subclassResponsibility 
]

{ #category : #printing }
IceRemote >> printOn: stream [
	stream 
		<< (self name ifNil: [ 'noname' ]) 
		<< ' (' << self url << ')'
]

{ #category : #accessing }
IceRemote >> projectBasename [
	^ self projectName withoutSuffix: '.git'
]

{ #category : #accessing }
IceRemote >> projectName [
	^ projectName
]

{ #category : #testing }
IceRemote >> referencesSameRemoteLocationAs: another [
	"Base/trivial implementation is that two urls are the same if they are equal, 
	but some subclasses have more complicated logic to detect two urls pointing to the same remote"
	^ self url sameAs: another url
]

{ #category : #branches }
IceRemote >> remoteBranchNamed: aString [ 
	
	^ self
		remoteBranchNamed: aString
		ifPresent: [ :branch | ^ branch ]
		ifAbsent: [ IceBranchNotFound new
			branchName: aString;
			signal ]
]

{ #category : #branches }
IceRemote >> remoteBranchNamed: aString ifAbsent: aBlock [
	
	^ self
		remoteBranchNamed: aString
		ifPresent: [ :branch | ^ branch ]
		ifAbsent: [ ^ aBlock value ]
]

{ #category : #branches }
IceRemote >> remoteBranchNamed: aString ifPresent: presentClosure ifAbsent: absentClosure [
	
	^ absentClosure value
]

{ #category : #accessing }
IceRemote >> upstreamForBranch: aBranch backend: backend [
	^ backend 
		lookupBranchNamed: aBranch name 
		inRemote: self remoteName

]

{ #category : #accessing }
IceRemote >> url [
	^ url
]

{ #category : #accessing }
IceRemote >> url: aString [
	self basicUrl: aString.
	self parseUrl
]
