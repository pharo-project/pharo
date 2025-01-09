Class {
	#name : 'AutomaticRewriting',
	#superclass : 'Warning',
	#instVars : [
		'context',
		'explanationString',
		'deprecationDate',
		'versionString',
		'rule',
		'condition'
	],
	#classInstVars : [
		'log',
		'raiseWarning',
		'showWarning',
		'active'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'logging' }
AutomaticRewriting class >> activateTransformations [

	^ active ifNil: [ active := true ]
]

{ #category : 'logging' }
AutomaticRewriting class >> activateTransformations: aBoolean [

	active := aBoolean
]

{ #category : 'logging' }
AutomaticRewriting class >> addLog: anElement [ 

	self log add: anElement
]

{ #category : 'class initialization' }
AutomaticRewriting class >> initialize [
	self reset
]

{ #category : 'class initialization' }
AutomaticRewriting class >> log [ 

	^ log ifNil: [ log := OrderedCollection new. log ]
]

{ #category : 'settings' }
AutomaticRewriting class >> raiseWarning [
	"If true, then a dialog is popup for each method invocation"
	^ raiseWarning ifNil: [ raiseWarning := false ]
]

{ #category : 'settings' }
AutomaticRewriting class >> raiseWarning: aBoolean [
	raiseWarning := aBoolean
]

{ #category : 'class initialization' }
AutomaticRewriting class >> reset [
	<script>
	log := nil.
	raiseWarning := nil.
	showWarning := nil.
	active := nil
]

{ #category : 'settings' }
AutomaticRewriting class >> showWarning [
	"If true, then a message is send to the Transcript for each deprecated method invocation"
	
	^ showWarning ifNil: [ showWarning := true ]
]

{ #category : 'settings' }
AutomaticRewriting class >> showWarning: aBoolean [
	
	showWarning := aBoolean
]

{ #category : 'comparing' }
AutomaticRewriting >> = anObject [
	^self class == anObject class
	  and: [context = anObject contextOfDeprecatedMethod
	  and: [context
			ifNil: [explanationString = anObject explanationString]
			ifNotNil: [true]]]
]

{ #category : 'accessing' }
AutomaticRewriting >> condition: aBlock [
	condition := aBlock
]

{ #category : 'accessing' }
AutomaticRewriting >> context: aContext [
	context := aContext
]

{ #category : 'accessing' }
AutomaticRewriting >> contextOfDeprecatedMethod [
	^context
]

{ #category : 'accessing' }
AutomaticRewriting >> contextOfSender [
	^context home sender
]

{ #category : 'accessing' }
AutomaticRewriting >> date: aDate [
	deprecationDate := aDate
]

{ #category : 'private' }
AutomaticRewriting >> deprecatedMethodName [
	^self contextOfDeprecatedMethod homeMethod printString
]

{ #category : 'accessing' }
AutomaticRewriting >> deprecationDate [

	^ deprecationDate ifNil: [ 'unknown' ]
]

{ #category : 'accessing' }
AutomaticRewriting >> explanation: aString [
	explanationString := aString
]

{ #category : 'accessing' }
AutomaticRewriting >> explanationString [

	^ explanationString
]

{ #category : 'comparing' }
AutomaticRewriting >> hash [
	^(context ifNil: [explanationString]) hash
]

{ #category : 'settings' }
AutomaticRewriting >> raiseWarning [
	^ self class raiseWarning
]

{ #category : 'private' }
AutomaticRewriting >> rewriterClass [
	^ self class environment at: #OCParseTreeRewriter ifAbsent: [ nil ]
]

{ #category : 'accessing' }
AutomaticRewriting >> rule: aRule [
	rule := aRule
]

{ #category : 'private' }
AutomaticRewriting >> sendingMethodName [
	^self contextOfSender homeMethod printString
]

{ #category : 'handling' }
AutomaticRewriting >> shouldTransform [

	self class activateTransformations ifFalse: [ ^false  ].
	(condition isNil or: [ condition cull: self ]) ifFalse: [ ^false ].
	^rule isNotNil
]

{ #category : 'settings' }
AutomaticRewriting >> showWarning [
	^ self class showWarning
]

{ #category : 'signaling' }
AutomaticRewriting >> signal [
	| pragma homeMethod |
	homeMethod := context homeMethod.
	(homeMethod hasPragmaNamed: #transform:to:) ifFalse: [ ^super signal ].

	pragma := homeMethod pragmaAt: #transform:to:.
	self rule: pragma arguments first -> pragma arguments second.
	self transform
]

{ #category : 'accessing' }
AutomaticRewriting >> version: aString [
	versionString := aString
]

{ #category : 'accessing' }
AutomaticRewriting >> versionString [

	^ versionString ifNil: [ 'unknown' ]
]
