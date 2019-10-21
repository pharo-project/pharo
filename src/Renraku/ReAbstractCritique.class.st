"
I am an abstract critic class.

My subclasses should define whether they provide a selection interval, or refactory change
"
Class {
	#name : #ReAbstractCritique,
	#superclass : #ReProperty,
	#instVars : [
		'rule',
		'tinyHint'
	],
	#category : #'Renraku-Critiques'
}

{ #category : #'instance creation' }
ReAbstractCritique class >> for: anEntity by: aRule [

	^ self basicNew
		initializeRule: aRule
		target: anEntity;
		yourself
]

{ #category : #'instance creation' }
ReAbstractCritique class >> withAnchor: anAnchor by: aRule [

	^ self basicNew
		initializeRule: aRule
		sourceAnchor: anAnchor;
		yourself
]

{ #category : #'special instance creation' }
ReAbstractCritique class >> withAnchor: anAnchor by: aRule hint: aString [

	^ (self
		withAnchor: anAnchor
		by: aRule)
		tinyHint: aString;
		yourself
]

{ #category : #actions }
ReAbstractCritique >> actions [ 

	| actions |
	
	actions := OrderedCollection new: 3.

	actions add: (RePropertyAction new
			icon: (self iconNamed: #smallQuestionIcon);
			description: 'View rationale behind the rule';
			action: [ :crit |
				crit popDescriptionUp.
				ReSystemAnnouncer uniqueInstance
					notifyCritique: crit
					descriptionViewedFor: crit sourceAnchor entity ];
			yourself).
	
	self providesChange ifTrue: [ 
		actions add: (RePropertyAction new
			icon: (self iconNamed: #repairIcon);
			description: 'Automatically resolve the issue';
			action: [ :crit |
				| changesBrowser |
				
				changesBrowser := (ChangesBrowser changes: { crit change })
					extent: 500@400;
					setModal: SystemWindow topWindow;
					yourself.
					
				changesBrowser openDialogWithSpec
					okAction: [ changesBrowser accept ];
					centered ];
			yourself) ].
	
	actions add: (RePropertyAction new
			icon: (self iconNamed: #smallCancelIcon);
			description: 'Ban this rule';
			action: [ :crit | crit guidedBan ];
			yourself).

	^ actions
]

{ #category : #actions }
ReAbstractCritique >> ban [
	self entity ban: self
]

{ #category : #accessing }
ReAbstractCritique >> change [
	
	^ self subclassResponsibility 
]

{ #category : #accessing }
ReAbstractCritique >> color [

	^ ({
	#error -> Color red .
	#warning -> Color yellow .
	#information -> Color blue
	 } asDictionary at: rule severity ifAbsent: [ super color ]) alpha: 0.1

]

{ #category : #accessing }
ReAbstractCritique >> description [
	^ rule rationale
]

{ #category : #actions }
ReAbstractCritique >> guidedBan [

	ReCriticEngine guidedBy: self entity  ban: self
]

{ #category : #accessing }
ReAbstractCritique >> icon [

	^ self iconNamed: ('small', rule severity capitalized, 'Icon') asSymbol
]

{ #category : #initialization }
ReAbstractCritique >> initializeRule: aRule sourceAnchor: anAnchor [

	self initializeSourceAnchor: anAnchor.
	rule := aRule.
]

{ #category : #initialization }
ReAbstractCritique >> initializeRule: aRule target: anEntity [

	self
		initializeRule: aRule
		sourceAnchor: (ReSourceAnchor entity: anEntity)
]

{ #category : #testing }
ReAbstractCritique >> isCritique [
	^ true
]

{ #category : #actions }
ReAbstractCritique >> popDescriptionUp [
	UIManager default longMessage: self description title: self title
]

{ #category : #printing }
ReAbstractCritique >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' (', self title, ')'
]

{ #category : #actions }
ReAbstractCritique >> provideNegativeFeedback: aComment [
	ReFeedbacker negativeFeedback: aComment About: self
]

{ #category : #actions }
ReAbstractCritique >> providePositiveFeedback: aComment [
	ReFeedbacker positiveFeedback: aComment About: self
]

{ #category : #testing }
ReAbstractCritique >> providesChange [

	^ self subclassResponsibility 
]

{ #category : #accessing }
ReAbstractCritique >> rule [
	^ rule
]

{ #category : #accessing }
ReAbstractCritique >> tinyHint [

	^ tinyHint
]

{ #category : #accessing }
ReAbstractCritique >> tinyHint: anObject [
	tinyHint := anObject
]

{ #category : #accessing }
ReAbstractCritique >> title [
	^ (self tinyHint isNil or: [ self tinyHint isEmpty ])
		ifTrue: [ rule name ]
		ifFalse: [
			String streamContents: [ :s |
				s
					nextPut: $[;
					nextPutAll: self tinyHint;
					nextPutAll: '] ';
					nextPutAll: rule name ] ]
]
