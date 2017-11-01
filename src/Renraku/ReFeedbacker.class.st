"
I am reporting complaints about critics. No one should use me dirrectly. Use ReCritic>>#reportUselessness instead
"
Class {
	#name : #ReFeedbacker,
	#superclass : #Object,
	#classInstVars : [
		'uuid'
	],
	#category : 'Renraku-Utility'
}

{ #category : #actions }
ReFeedbacker class >> feedback: aComment positive: aBoolean About: aReCritic [ 
	[ 
	ZnClient new
		beOneShot;
		http;
		host: 'renraku.inf.usi.ch';
		addPath: 'feedback';
		formAt: 'uuid' put: self uuid asString;
		formAt: 'pharoVersion' put: SystemVersion current highestUpdate asString;
		formAt: 'author' put: Author fullName;
		formAt: 'rule' put: aReCritic rule class name;
		formAt: 'target' put: aReCritic sourceAnchor entity name;
		formAt: 'time' put: DateAndTime now asString;
		formAt: 'positive' put: aBoolean asString;
		formAt: 'comment' put: aComment;
		post ]
		on: NetworkError
		do: [ :ex | ^ false ].
		
	^ true
]

{ #category : #actions }
ReFeedbacker class >> negativeFeedback: aComment About: aReCritic [
	
	^ self feedback: aComment positive: false About: aReCritic
]

{ #category : #actions }
ReFeedbacker class >> positiveFeedback: aComment About: aReCritic [
	
	^ self feedback: aComment positive: true About: aReCritic
]

{ #category : #properties }
ReFeedbacker class >> uuid [
	^ uuid ifNil: [ uuid := UUID new ]
]
