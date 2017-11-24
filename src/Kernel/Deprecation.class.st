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
	#name : #Deprecation,
	#superclass : #Warning,
	#instVars : [
		'context',
		'explanationString',
		'deprecationDate',
		'versionString',
		'rule'
	],
	#classVars : [
		'Log',
		'RaiseWarning',
		'ShowWarning'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #logging }
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

{ #category : #'class initialization' }
Deprecation class >> initialize [
	Log := nil "#deprecationsWhile: logs all deprecations here"
]

{ #category : #settings }
Deprecation class >> raiseWarning [
	"If true, then a dialog is popup for each deprecated method invocation"
	^ RaiseWarning ifNil: [RaiseWarning := true]
]

{ #category : #settings }
Deprecation class >> raiseWarning: aBoolean [
	RaiseWarning := aBoolean
]

{ #category : #settings }
Deprecation class >> showWarning [
	"If true, then a message is send to the Transcript for each deprecated method invocation"
	^ ShowWarning ifNil: [ShowWarning := true]
]

{ #category : #settings }
Deprecation class >> showWarning: aBoolean [
	ShowWarning := aBoolean
]

{ #category : #comparing }
Deprecation >> = anObject [
	^self class == anObject class
	  and: [context = anObject context
	  and: [context
			ifNil: [explanationString = anObject explanationString]
			ifNotNil: [true]]]
]

{ #category : #accessing }
Deprecation >> context: aContext [
	context := aContext
]

{ #category : #accessing }
Deprecation >> contextOfDeprecatedMethod [
	^context
]

{ #category : #accessing }
Deprecation >> contextOfSender [
	^context sender
]

{ #category : #accessing }
Deprecation >> date: aDate [
	deprecationDate := aDate
]

{ #category : #handling }
Deprecation >> defaultAction [
	Log 
		ifNotNil: [:log | log add: self].
	self logTranscript.
	self raiseWarning 
		ifTrue: [super defaultAction]
]

{ #category : #private }
Deprecation >> deprecatedMethodName [
	^self contextOfDeprecatedMethod method printString
]

{ #category : #accessing }
Deprecation >> deprecationDate [

	^ deprecationDate ifNil: [ 'unknown' ]
]

{ #category : #accessing }
Deprecation >> explanation: aString [
	explanationString := aString 
]

{ #category : #accessing }
Deprecation >> explanationString [

	^ explanationString
]

{ #category : #comparing }
Deprecation >> hash [
	^(context ifNil: [explanationString]) hash
]

{ #category : #handling }
Deprecation >> logTranscript [
	self showWarning 
		ifTrue: [Transcript nextPutAll: self messageText; cr; flush].
]

{ #category : #accessing }
Deprecation >> messageText [
	"Return an exception's message text."

	^ 'The method ', self deprecatedMethodName, ' called from ', self sendingMethodName ,' has been deprecated.
', explanationString
]

{ #category : #settings }
Deprecation >> raiseWarning [
	^ self class raiseWarning
]

{ #category : #private }
Deprecation >> rewriterClass [
	^ self class environment at: #RBParseTreeRewriter ifAbsent: [ nil ]
]

{ #category : #accessing }
Deprecation >> rule: aRule [
	rule := aRule
]

{ #category : #private }
Deprecation >> sendingMethodName [
	^self contextOfSender method printString
]

{ #category : #settings }
Deprecation >> showWarning [
	^ self class showWarning
]

{ #category : #handling }
Deprecation >> transform [
	| node rewriteRule aMethod |
	self raiseWarning ifFalse: [ ^ self ]. "when desprecations are disables, we do not want to transform, either"
	self rewriterClass ifNil:[ ^ self signal ].
	aMethod := self contextOfSender method.
	aMethod isDoIt ifTrue:[^ self]. "no need to transform doits"
	node := self contextOfSender sourceNodeExecuted.
	rewriteRule := self rewriterClass new 
		replace: rule key with: rule value.
	(rewriteRule executeTree: node)
		ifFalse: [ ^ self ].
	node replaceWith: rewriteRule tree. 
	Author 
		useAuthor: 'AutoDeprecationRefactoring'
		during: [aMethod origin compile: aMethod ast formattedCode classified: aMethod protocol].	
	Log 
		ifNotNil: [:log | log add: self].
	self logTranscript
]

{ #category : #accessing }
Deprecation >> version: aString [
	versionString := aString
]

{ #category : #accessing }
Deprecation >> versionString [

	^ versionString ifNil: [ 'unknown' ]
]
