"
This smell arises when a method is implemented but never sent. If a method is not sent, it can be removed. This rule pays attention not to identify as unsent methods, methods with pragmas and test methods since they are likely to be sent through reflection.
	Now if your code is used and extended by others better use a deprecation mechanism. To define a deprecate method follow the pattern: 
	
	foo
		self deprecated: ''Use bar instead ''. 
		^ self bar
		 
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
					self forApproxMessagesOf: method do: [ :mes |
						allMessages add: mes ] ] ].
		allMessages	]
]

{ #category : #testing }
ReImplementedNotSentRule class >> checksMethod [
	^ true 
]

{ #category : #cleanup }
ReImplementedNotSentRule class >> cleanUp [

	allMessages := nil
]

{ #category : #accessing }
ReImplementedNotSentRule class >> forApproxMessagesOf: aMethod do: aBlock [
	"this is so ugly purely for performance reasons"
	"as oposed to the #messages method of the CompiledCode class
	this approach is 11 times faster and had 87% precision and
	100% recall in the standard pharo image upon method creation"

	"literals are shifted by 1 right (that's why we start from 2"
	"2 last literalls are method name and class name
	(that's why we go to num - 1 (had to be -2 but mind the shift))"
	2 to: aMethod numLiterals - 1 do: [ :i |
		| l |
		l := aMethod objectAt: i.
		"besides symbols literals may be associations for class reffs
		or other types of the actual literals"
		l isSymbol ifTrue: [ aBlock value: l ] ]
]

{ #category : #initialization }
ReImplementedNotSentRule class >> initialize [
	
	self subscribe
]

{ #category : #'system announcements' }
ReImplementedNotSentRule class >> methodAdded: anAnnouncement [

	allMessages := nil
]

{ #category : #'system announcements' }
ReImplementedNotSentRule class >> methodModified: anAnnouncement [

	allMessages := nil
]

{ #category : #'system announcements' }
ReImplementedNotSentRule class >> methodRemoved: anAnnouncement [

	allMessages := nil
]

{ #category : #announcement }
ReImplementedNotSentRule class >> subscribe [
	<systemEventRegistration>
	self unsubscribe.
	
	SystemAnnouncer uniqueInstance weak
		when: MethodAdded    send: #methodAdded: to: self;
		when: MethodRemoved  send: #methodRemoved: to: self;
		when: MethodModified send: #methodModified: to: self
]

{ #category : #accessing }
ReImplementedNotSentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ImplementedNotSentRule'
]

{ #category : #announcement }
ReImplementedNotSentRule class >> unsubscribe [

	SystemAnnouncer uniqueInstance unsubscribe: self.
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
