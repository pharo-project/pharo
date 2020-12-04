"
This smell arises when a method is implemented but never sent. If a method is not sent, it can be removed. This rule pays attention not to identify as unsent methods, methods with pragmas and test methods since they are likely to be sent through reflection.
	Now if your code is used and extended by others better use a deprecation mechanism. To define a deprecate method follow the pattern: 
	
	foo
		self deprecated: ''Use bar instead ''. 
		^ self bar
		 
NOTE: the rule updates its data with messages send in new methods, but does not remove messages till the next image startup. The reason is that it would be to slow to re-calculate the data at each method removal.

It can be forced to update by executing:

	ReImplementedNotSentRule reset
"
Class {
	#name : #ReImplementedNotSentRule,
	#superclass : #ReAbstractRule,
	#classInstVars : [
		'allMessages'
	],
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReImplementedNotSentRule class >> allMessages [
	"return all 'message sends' in the system and cache them"

	^allMessages ifNil: [
		allMessages := IdentitySet new.
		Smalltalk globals allBehaviors do: [ :behavior | 
				behavior methodsDo: [ :method |
						allMessages addAll: method messages ] ].
		allMessages	]
]

{ #category : #testing }
ReImplementedNotSentRule class >> checksMethod [
	^ true 
]

{ #category : #cleanup }
ReImplementedNotSentRule class >> cleanUp [
	self reset
]

{ #category : #accessing }
ReImplementedNotSentRule class >> enabled [
	^ enabled ifNil: [ enabled := false ]
]

{ #category : #accessing }
ReImplementedNotSentRule class >> enabled: aBoolean [
	super enabled: aBoolean.
	self reset.
	aBoolean 
		ifTrue: [ self subscribe ]
		ifFalse: [ self unsubscribe ]
]

{ #category : #initialization }
ReImplementedNotSentRule class >> initialize [
	"we do not want to register if the rule is disabled"
	self enabled ifFalse: [ ^self ].
	self reset.
	self subscribe.
	SessionManager default 
		registerToolClassNamed: self name
]

{ #category : #'system announcements' }
ReImplementedNotSentRule class >> methodAdded: anAnnouncement [

	self allMessages addAll: anAnnouncement methodAdded messages
]

{ #category : #'system announcements' }
ReImplementedNotSentRule class >> methodModified: anAnnouncement [

	self allMessages addAll: anAnnouncement newMethod messages
]

{ #category : #cleanup }
ReImplementedNotSentRule class >> reset [
	<script>
	allMessages := nil
]

{ #category : #cleanup }
ReImplementedNotSentRule class >> shutDown [	
	self reset
]

{ #category : #announcement }
ReImplementedNotSentRule class >> subscribe [
	<systemEventRegistration>
	self unsubscribe.
	
	SystemAnnouncer uniqueInstance weak
		when: MethodAdded    send: #methodAdded: to: self;
		when: MethodModified send: #methodModified: to: self
]

{ #category : #accessing }
ReImplementedNotSentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ImplementedNotSentRule'
]

{ #category : #announcement }
ReImplementedNotSentRule class >> unsubscribe [

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #running }
ReImplementedNotSentRule >> basicCheck: aMethod [
	"Check if there are any senders. Furthermore methods with pragmas are likely to be sent through reflection, thus do not report those."

	aMethod pragmas ifNotEmpty: [ ^ false ].
	aMethod isTestMethod ifTrue: [ ^ false ].

	^ (self class allMessages includes: aMethod selector) not
]

{ #category : #accessing }
ReImplementedNotSentRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReImplementedNotSentRule >> name [
	^ 'Methods implemented but not sent'
]

{ #category : #accessing }
ReImplementedNotSentRule >> severity [
	^ #information
]
