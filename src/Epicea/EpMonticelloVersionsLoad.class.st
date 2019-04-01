"
I represent a Monticello ""Load Versions"" operation performed in the system.
"
Class {
	#name : #EpMonticelloVersionsLoad,
	#superclass : #EpEvent,
	#instVars : [
		'versionNames'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpMonticelloVersionsLoad class >> versionNames: aCollectionOfVersionNames [

	^ self basicNew
		initializeWith: aCollectionOfVersionNames;
		yourself
]

{ #category : #'instance creation' }
EpMonticelloVersionsLoad class >> versions: aCollectionOfVersions [ 

	^ self versionNames: (aCollectionOfVersions collect: [ :version | version info name ])
]

{ #category : #comparing }
EpMonticelloVersionsLoad >> = anObject [

	^ (self class = anObject class)
		and: [ self versionNames = anObject versionNames ]
]

{ #category : #visitor }
EpMonticelloVersionsLoad >> accept: aChangeVisitor [ 
	^ aChangeVisitor visitMonticelloVersionsLoad: self
]

{ #category : #accessing }
EpMonticelloVersionsLoad >> affectedPackageName [
	self flag: #pharoFixMe.	"FIX: imprecise answer"

	^ self packageNames first
]

{ #category : #comparing }
EpMonticelloVersionsLoad >> hash [

	^ self class hash
		bitXor: self versionNames hash
]

{ #category : #initializing }
EpMonticelloVersionsLoad >> initializeWith: aCollectionOfVersionNames [
	self initialize.
	versionNames := aCollectionOfVersionNames
]

{ #category : #testing }
EpMonticelloVersionsLoad >> isMonticelloVersionsLoad [

	^ true
]

{ #category : #accessing }
EpMonticelloVersionsLoad >> packageNames [

	^ versionNames collect: [ :each | each copyUpToLast: $- ]
]

{ #category : #printing }
EpMonticelloVersionsLoad >> printOn: aStream [

	super printOn: aStream.
	versionNames printElementsOn: aStream.
]

{ #category : #accessing }
EpMonticelloVersionsLoad >> versionNames [
	^ versionNames
]
