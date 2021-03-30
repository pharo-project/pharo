"
A ""step"" in spotter is part of the nested structure created by diving into a category.
""Diving"" means enter to a particular category (or ""processor"" in spotter definition) to show more in detail its results.
The old spotter allowed also other kinds of diving, but current implementation removed that (since it was not really used).
"
Class {
	#name : #StSpotterStep,
	#superclass : #Object,
	#instVars : [
		'model',
		'origin',
		'activeQuery',
		'processors'
	],
	#category : #'NewTools-Spotter-Model'
}

{ #category : #'instance creation' }
StSpotterStep class >> newModel: aSpotterModel [

	^ self new 
		model: aSpotterModel;
		yourself
]

{ #category : #accessing }
StSpotterStep >> activate [

	activeQuery ifNil: [ ^ self ].
	self activateQuery: activeQuery
]

{ #category : #private }
StSpotterStep >> activateQuery: aQuery [

	activeQuery ifNotNil: [ self disconnectQuery: aQuery ].
	activeQuery := aQuery.
	self connectQuery: aQuery.
	self refreshQueryStatus: aQuery
]

{ #category : #accessing }
StSpotterStep >> activeQuery [

	^ activeQuery
]

{ #category : #accessing }
StSpotterStep >> addProcessor: processor [
	
	processors add: processor
]

{ #category : #accessing }
StSpotterStep >> candidatesList [

	^ activeQuery 
		ifNotNil: [ :aQuery | aQuery candidatesList ]
		ifNil: [ #() ]
]

{ #category : #private }
StSpotterStep >> connectQuery: aQuery [

	aQuery announcer
		when: StSpotterCandidateAdded send: #announceCandidateAdded: to: self model;
		when: StSpotterAllCandidatesAdded send: #announceAllCandidatesAdded: to: self model;
		when: StSpotterCandidatesAmountChanged send: #announceAmountChanged: to: self model;
		when: StSpotterQueryStarted send: #announceQueryStarted: to: self model;
		when: StSpotterQueryEnded send: #announceQueryEnded: to: self model.
]

{ #category : #accessing }
StSpotterStep >> deactivate [

	self activeQuery processors do: [ :each | each stop ].
	self disconnectQuery: activeQuery
]

{ #category : #private }
StSpotterStep >> disconnectQuery: aQuery [

	aQuery announcer unsubscribe: self model
]

{ #category : #accessing }
StSpotterStep >> lastSearchText [

	^ self activeQuery searchText
]

{ #category : #accessing }
StSpotterStep >> links [

	^ activeQuery 
		ifNotNil: [ :aQuery | aQuery links ]
		ifNil: [ #() ]
]

{ #category : #factory }
StSpotterStep >> listProcessor [
	| processor |
	
	processor := StSpotterCandidatesListProcessor new.
	self addProcessor: processor.
	
	^ processor
]

{ #category : #accessing }
StSpotterStep >> model [

	^ model
]

{ #category : #accessing }
StSpotterStep >> model: aSpotterModel [

	model := aSpotterModel
]

{ #category : #accessing }
StSpotterStep >> origin [

	^ origin
]

{ #category : #accessing }
StSpotterStep >> origin: anObject [

	origin := anObject
]

{ #category : #accessing }
StSpotterStep >> previousProcessorFrom: aCollection [

	aCollection do: [ :each | 
		" we assume they are all identical "
		each asStSpotterCandidateLink processor ifNotNil: [ ^ each processor ] ]. 
	
	^ StSpotterCandidatesListProcessor new
		title: 'Items';
		filter: StFilterSubstring;
		yourself
]

{ #category : #processing }
StSpotterStep >> process: aString [
	| query |

	query := StSpotterQuery on: (aString ifNotEmpty: [ aString, ' ' ]).
	query processors: (self processors sort: #order ascending).
	self activateQuery: query.
	query process.
	query candidatesList isEmpty 
		ifTrue: [ self model announceNoResults ]
]

{ #category : #accessing }
StSpotterStep >> processors [

	^ processors ifNil: [ 
		processors := OrderedCollection new.
		self origin stSpotterProcessorsFor: self.
		processors ]
]

{ #category : #accessing }
StSpotterStep >> processors: aCollection [

	processors := aCollection
]

{ #category : #private }
StSpotterStep >> refreshQueryStatus: aQuery [

	aQuery links ifEmpty: [ ^ self ].

	self model announceAllCandidatesAdded: (StSpotterAllCandidatesAdded new
		candidateLinks: aQuery links;
		yourself).

	aQuery isFinished ifTrue: [ 
		self model announceAmountChanged: (StSpotterCandidatesAmountChanged new 
			amount: aQuery links size;
			yourself) ]
]

{ #category : #accessing }
StSpotterStep >> results [

	^ activeQuery 
		ifNotNil: [ :aQuery | aQuery results ]
		ifNil: [ #() ]
]
