"
This Warning is signalled by methods which are deprecated.

Idiom: Imagine I want to deprecate the message #foo.

foo
	^ 'foo'

I can replace it with:

foo
	self 
		deprecated:   'The method #foo was not good. Use Bar>>newFoo instead.'
		on:  'here add date'
		in:  'here add version'
		 transformWith:   '`@receiver foo' -> '`@receiver newFoo'.	
	^self newFoo
	
	
The  transformWith:  part is optional. It allows to transform the deprecated method automatically when called.
If the transformation is defined, the Warning will not signalled.


"
Class {
	#name : 'Deprecation',
	#superclass : 'Warning',
	#instVars : [
		'context',
		'explanationString',
		'deprecationDate',
		'versionString',
		'rule',
		'condition'
	],
	#classVars : [
		'Active',
		'Log',
		'RaiseWarning',
		'ShowWarning'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'logging' }
Deprecation class >> activateTransformations [

	^ Active ifNil: [ Active := true ]
]

{ #category : 'logging' }
Deprecation class >> activateTransformations: aBoolean [

	Active := aBoolean
]

{ #category : 'logging' }
Deprecation class >> deprecationsWhile: aBlock [
	"returns a log of all deprecated methods seen while executing aBlock"
	| oldLog result |
	oldLog := Log.
	Log := Set new.
	aBlock value.
	result := Log.
	oldLog ifNotNil: [oldLog addAll: result].
	Log := oldLog.
	^result
]

{ #category : 'class initialization' }
Deprecation class >> initialize [
	self reset
]

{ #category : 'settings' }
Deprecation class >> raiseWarning [
	"If true, then a dialog is popup for each deprecated method invocation"
	^ RaiseWarning ifNil: [RaiseWarning := false]
]

{ #category : 'settings' }
Deprecation class >> raiseWarning: aBoolean [
	RaiseWarning := aBoolean
]

{ #category : 'class initialization' }
Deprecation class >> reset [
	<script>
	Log := nil.
	RaiseWarning := nil.
	ShowWarning := nil.
	Active := nil
]

{ #category : 'settings' }
Deprecation class >> showWarning [
	"If true, then a message is send to the Transcript for each deprecated method invocation"
	^ ShowWarning ifNil: [ShowWarning := true]
]

{ #category : 'settings' }
Deprecation class >> showWarning: aBoolean [
	ShowWarning := aBoolean
]

{ #category : 'comparing' }
Deprecation >> = anObject [
	^self class == anObject class
	  and: [context = anObject contextOfDeprecatedMethod
	  and: [context
			ifNil: [explanationString = anObject explanationString]
			ifNotNil: [true]]]
]

{ #category : 'accessing' }
Deprecation >> condition: aBlock [
	condition := aBlock
]

{ #category : 'accessing' }
Deprecation >> context: aContext [
	context := aContext
]

{ #category : 'accessing' }
Deprecation >> contextOfDeprecatedMethod [
	^context
]

{ #category : 'accessing' }
Deprecation >> contextOfSender [
	^context home sender
]

{ #category : 'accessing' }
Deprecation >> date: aDate [
	deprecationDate := aDate
]

{ #category : 'handling' }
Deprecation >> defaultAction [

	Log ifNotNil: [ :log | log add: self ].
	self showWarning ifTrue: [ self logTranscript ].
	self raiseWarning ifTrue: [ super defaultAction ].
	self shouldTransform ifTrue: [ self transform ]
]

{ #category : 'private' }
Deprecation >> deprecatedMethodName [
	^self contextOfDeprecatedMethod homeMethod printString
]

{ #category : 'accessing' }
Deprecation >> deprecationDate [

	^ deprecationDate ifNil: [ 'unknown' ]
]

{ #category : 'accessing' }
Deprecation >> explanation: aString [
	explanationString := aString
]

{ #category : 'accessing' }
Deprecation >> explanationString [

	^ explanationString
]

{ #category : 'comparing' }
Deprecation >> hash [
	^(context ifNil: [explanationString]) hash
]

{ #category : 'private' }
Deprecation >> logTranscript [
	DeprecationPerformedNotification signal: self messageText
]

{ #category : 'accessing' }
Deprecation >> messageText [
	^String streamContents: [ :str |
		self shouldTransform ifTrue: [
			str nextPutAll:  'Automatic deprecation code rewrite: '].
		str
			nextPutAll: 'The method ';
			nextPutAll: self deprecatedMethodName;
			nextPutAll: ' called from ';
			nextPutAll: self sendingMethodName;
			nextPutAll: ' has been deprecated. ';
		 	nextPutAll: explanationString]
]

{ #category : 'settings' }
Deprecation >> raiseWarning [
	^ self class raiseWarning
]

{ #category : 'private' }
Deprecation >> rewriterClass [
	^ self class environment at: #RBParseTreeRewriter ifAbsent: [ nil ]
]

{ #category : 'accessing' }
Deprecation >> rule: aRule [
	rule := aRule
]

{ #category : 'private' }
Deprecation >> sendingMethodName [
	^self contextOfSender homeMethod printString
]

{ #category : 'handling' }
Deprecation >> shouldTransform [
	self class activateTransformations ifFalse: [ ^false  ].
	(condition isNil or: [ condition cull: self ]) ifFalse: [ ^false ].
	^rule isNotNil
]

{ #category : 'settings' }
Deprecation >> showWarning [
	^ self class showWarning
]

{ #category : 'handling' }
Deprecation >> signal [
	| pragma homeMethod |
	homeMethod := context homeMethod.
	(homeMethod hasPragmaNamed: #transform:to:) ifFalse: [ ^super signal ].

	pragma := homeMethod pragmaAt: #transform:to:.
	self rule: pragma arguments first -> pragma arguments second.
	self transform
]

{ #category : 'handling' }
Deprecation >> transform [

	| node rewriteRule aMethod |

	self rewriterClass ifNil:[ ^ self ].
	aMethod := self contextOfSender homeMethod.
	aMethod isDoIt ifTrue:[ ^ self ]. "no need to transform doits"
	node := self contextOfSender sourceNodeExecuted.
	RecursionStopper during: [
		rewriteRule := self rewriterClass new
			replace: rule key with: rule value.
		(rewriteRule executeTree: node)
			ifFalse: [ ^ self ].
		node replaceWith: rewriteRule tree.
		aMethod origin
			compile: aMethod ast formattedCode
			classified: aMethod protocol.
		Log ifNotNil: [ :log | log add: self ] ]
]

{ #category : 'accessing' }
Deprecation >> version: aString [
	versionString := aString
]

{ #category : 'accessing' }
Deprecation >> versionString [

	^ versionString ifNil: [ 'unknown' ]
]
