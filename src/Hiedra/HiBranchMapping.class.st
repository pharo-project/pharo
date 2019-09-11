"
I know how to map each link and node of a Hiedra ruler to a branch number. This is useful to colorize a ruler.
Note that my mapping is arbitrary, and other mappings are possible.
"
Class {
	#name : #HiBranchMapping,
	#superclass : #Object,
	#instVars : [
		'ruler',
		'branchByLink',
		'branchCount',
		'branchByNode'
	],
	#category : #'Hiedra-Model'
}

{ #category : #convenience }
HiBranchMapping class >> builtFor: aHiRuler [ 
	^ (self newFor: aHiRuler)
		build;
		yourself
]

{ #category : #'instance creation' }
HiBranchMapping class >> newFor: aHiRuler [ 
	^ self basicNew
		initializeWith: aHiRuler;
		yourself
]

{ #category : #convenience }
HiBranchMapping >> allLinksWithSameBranchAs: aHiLink [ 
	| branch |
	branch := self branchAtLink: aHiLink.
	^ Array streamContents: [:stream |
		branchByLink keysAndValuesDo: [:link :branchNumber |
			branchNumber = branch ifTrue: [ stream nextPut: link ] ] ].

]

{ #category : #accessing }
HiBranchMapping >> branchAtLink: aHiLink [
	"Answer the branch number for a link."

	^ self branchByLink at: aHiLink
]

{ #category : #accessing }
HiBranchMapping >> branchAtNode: aHiNode [
	"Answer the branch number for a node."

	^ self branchByNode at: aHiNode
]

{ #category : #accessing }
HiBranchMapping >> branchByLink [
	^ branchByLink
]

{ #category : #accessing }
HiBranchMapping >> branchByNode [
	^ branchByNode
]

{ #category : #accessing }
HiBranchMapping >> branchCount [
	^ branchCount
]

{ #category : #building }
HiBranchMapping >> build [
	"Build the internal collaborators that will know the branch numbers for each node and link in the ruler. Required after instance creation."
	
	branchCount := 0.
	self buildBranchByLink.
	self buildBranchByNode.
]

{ #category : #building }
HiBranchMapping >> buildBranchByLink [
	"Build a dictionary that maps each link to a branch number."
	
	branchByLink := Dictionary new.
	ruler nodes reverseDo: [ :node |
		node incomingLinks do: [ :link |
			self markBranchLinksFrom: link ] ]
]

{ #category : #building }
HiBranchMapping >> buildBranchByNode [
	"Build a dictionary that maps each node to a branch number."
	
	branchByNode := Dictionary new.
	ruler nodes reverseDo: [ :node |
		self markNode: node ].
]

{ #category : #initialization }
HiBranchMapping >> initializeWith: aHiRuler [
	self initialize.
	ruler := aHiRuler
]

{ #category : #private }
HiBranchMapping >> markBranchLinksFrom: link [
	| nextLink |

	"Stop if the link is already marked."
	(branchByLink includesKey: link) ifTrue: [ ^self ].

	"Then, it's a new branch."
	branchCount := branchCount + 1.

	"Iterate the first available branch until there isn't any more."
	nextLink := link.
	[	branchByLink at: nextLink put: branchCount.
		nextLink origin incomingLinks ifEmpty: [ ^self ].
		nextLink := nextLink origin incomingLinks
			detect: [ :each | (branchByLink includesKey: each) not ]
			ifNone: [ ^self ]
	] repeat
]

{ #category : #private }
HiBranchMapping >> markNode: aHiNode [
	| branch |
	branch := aHiNode hasAnyLink 
		ifTrue: [ branchCount := branchCount + 1 ]
		ifFalse: [ self branchAtLink: aHiNode anyLink ].
	branchByNode
		at: aHiNode
		put: branch
]

{ #category : #accessing }
HiBranchMapping >> ruler [
	^ ruler
]
