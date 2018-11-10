"
I'm a proxy to keep information on pre-loaded packages when repository is not present.
This is the case of pharo project, which is distributed without a repository connection, but users may still need to link with it to make contributions.
"
Class {
	#name : #IceProxyMCVersionInfo,
	#superclass : #ProtoObject,
	#instVars : [
		'commitId',
		'packageName',
		'remoteUrl',
		'stubVersionInfo'
	],
	#classVars : [
		'LastCommitFound'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #private }
IceProxyMCVersionInfo class >> lastCommitFound [
	^ LastCommitFound
]

{ #category : #private }
IceProxyMCVersionInfo class >> lastCommitFound: aCommit [
	| oldCommit |
	oldCommit := LastCommitFound.
	LastCommitFound := aCommit.
	"This is just a cleanup mechanism... in general iceberg will be scanning 
	 a full repository and each proxy will be pointing to same commit so no 
	 point in keep it waiting"
	(LastCommitFound notNil and: [ oldCommit isNil ]) ifTrue: [ 
		[ (Delay forSeconds: 60) wait. 
		 	LastCommitFound := nil ] fork ]
]

{ #category : #'instance creation' }
IceProxyMCVersionInfo class >> remoteUrl: anUrl packageName: aName commit: aCommit message: aString author: authorString [
	^ self 
		remoteUrl: anUrl
		packageName: aName 
		commitId: aCommit id
		stub: (MCVersionInfo
			name: aName
			id: UUID new
			message: aString
			date: aCommit datetime asDate
			time: aCommit datetime asTime
			author: authorString
			ancestors: #())
]

{ #category : #'instance creation' }
IceProxyMCVersionInfo class >> remoteUrl: urlString packageName: nameString commitId: idString [
	^ self new 
		remoteUrl: urlString 
		packageName: nameString 
		commitId: idString
		stub: nil
]

{ #category : #'instance creation' }
IceProxyMCVersionInfo class >> remoteUrl: urlString packageName: nameString commitId: idString stub: aVersionInfo [
	^ self new 
		remoteUrl: urlString 
		packageName: nameString 
		commitId: idString
		stub: aVersionInfo
]

{ #category : #converting }
IceProxyMCVersionInfo >> asSpotterCandidateLink [
	^ GTSpotterCandidateLink value: self
]

{ #category : #'reflective operations' }
IceProxyMCVersionInfo >> becomeForward: otherObject copyHash: copyHash [
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

	(Array with: self)
		elementsForwardIdentityTo: (Array with: otherObject)
		copyHash: copyHash
]

{ #category : #'reflective operations' }
IceProxyMCVersionInfo >> doesNotUnderstand: aMessage [
	^ aMessage sendTo: (self findVersionInfo 
		ifNotNil: [ :versionInfo | self becomeForward: versionInfo copyHash: false ]
		ifNil: [ self stubVersionInfo ])
]

{ #category : #private }
IceProxyMCVersionInfo >> findVersionInfo [ 
	| repo remote commit |

	(self class lastCommitFound notNil 
		and: [ self class lastCommitFound id = commitId ])
		ifTrue: [ 
			commit := self class lastCommitFound.
			repo := commit repository ]
		ifFalse: [ 
			remote := IceGitRemote url: remoteUrl.
			repo := IceRepository registry 
				detect: [ :eachRepo | 
					eachRepo remotes 
						anySatisfy: [ :each | each referencesSameRemoteLocationAs: remote ] ]
				ifNone: [ ^ nil ].
			commit := repo lookupCommit: commitId.
			commit ifNil: [ 
				IceError signal: ('Your repository seems to be out of sync.!
Please fetch from "{1}" and try again.' 
					format: { remote }) ].
			self class lastCommitFound: commit ].
	
	^ IceMCVersionInfo new 
		fromCommit: commit 
		package: (IcePackage named: packageName repository: repo)
]

{ #category : #printing }
IceProxyMCVersionInfo >> gtDisplayOn: stream [
	"This offers a means to customize how the object is shown in the inspector"
	^ self printOn: stream
]

{ #category : #printing }
IceProxyMCVersionInfo >> gtDisplayString [
	"This offers a means to customize how the object is shown in the inspector.
	Do not override this method. Override gtDisplayOn: instead"
		
	| limitedString limit |
	limit := 1000.
	limitedString := String streamContents: [:s | self gtDisplayOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , ' ...'
]

{ #category : #inspecting }
IceProxyMCVersionInfo >> inspect [
	"Create and schedule an Inspector in which the user can examine the receiver's variables."
	^ Smalltalk tools inspector inspect: self
]

{ #category : #initialization }
IceProxyMCVersionInfo >> lazy [
	^ self
]

{ #category : #printing }
IceProxyMCVersionInfo >> printOn: aStream [
	aStream << self class name << '(' << packageName << ')'
]

{ #category : #printing }
IceProxyMCVersionInfo >> printString [
	^ String streamContents: [ :stream | self printOn: stream ]
]

{ #category : #initialization }
IceProxyMCVersionInfo >> remoteUrl: urlString packageName: nameString commitId: idString stub: aStub [
	remoteUrl := urlString.
	packageName := nameString.
	commitId := idString.
	stubVersionInfo := aStub
]

{ #category : #accessing }
IceProxyMCVersionInfo >> stubVersionInfo [ 
	^ stubVersionInfo ifNil: [ 
		stubVersionInfo := MCVersionInfo
			name: packageName
			id: UUID new
			message: 'fabricated from a proxy repository'
			date: nil
			time: nil
			author: ''
			ancestors: #() ]
]

{ #category : #accessing }
IceProxyMCVersionInfo >> stubVersionInfo: aVersionInfo [ 
	stubVersionInfo := aVersionInfo
]
