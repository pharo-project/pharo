"
This Warning is signalled by methods which are kept for backward compatible concerns.
Now you should not call them, and we nicely propose you that the system rewrite your messages to invoke the correct ones. 

Idiom: Imagine that you want to declare that a method is just kept around for backward compatibility.

```
foo
	^ 'foo'
```

For this we introduce the following call in the definition of the `foo` method:

```
foo
	self 
		backwardCompatible:  'The method #foo was not good. Use Bar>>newFoo instead.'
		on:  'here add date'
		in:  'here add version'
		transformWith: '`@receiver foo' -> '`@receiver newFoo'.	
	^ self newFoo
```

You are done the system will rewrite the callers to this foo method (and not other potential implementations) when it is executed. 
	
The `transformWith:`  allows one to transform the backard compatible method calls automatically when called.
The Warning will not signalled.

Check class side for posible configurations. You can ignore, raise warning, log or rewrite.


"
Class {
	#name : 'BackwardCompatibility',
	#superclass : 'AutomaticRewriting',
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'logging' }
BackwardCompatibility class >> backwardsCompatibilityWhile: aBlock [
	"Returns a log of all backward compatible methods seen while executing aBlock"

	| oldLog result |
	oldLog := log.
	log := Set new.
	aBlock value.
	result := log.
	oldLog ifNotNil: [ oldLog addAll: result ].
	log := oldLog.
	^ result
]

{ #category : 'handling' }
BackwardCompatibility >> defaultAction [

	self class addLog: self.
	self showWarning ifTrue: [ self logTranscript ].
	self raiseWarning ifTrue: [ super defaultAction ].
	self shouldTransform ifTrue: [ self transform ]
]

{ #category : 'private' }
BackwardCompatibility >> logTranscript [

	BackwardCompatibleRewritingCallPerformedNotification signal: self messageText
]

{ #category : 'accessing' }
BackwardCompatibility >> messageText [
	
	^ String streamContents: [ :str |
		self shouldTransform ifTrue: [
			str nextPutAll:  'Automatic backward compatible senders rewrite: '].
		str
			nextPutAll: 'The method ';
			nextPutAll: self deprecatedMethodName;
			nextPutAll: ' called from ';
			nextPutAll: self sendingMethodName;
			nextPutAll: ' is kept for backward compatible concerns.';
		 	nextPutAll: explanationString]
]

{ #category : 'handling' }
BackwardCompatibility >> transform [

	| node rewriteRule aMethod |

	self rewriterClass ifNil:[ ^ self ].
	aMethod := self contextOfSender homeMethod.
	
	"no need to transform doits or non installed methods.
	Non installed methods can arise if we are rewriting a closure whose home method was rewritten"
	aMethod isDoIt ifTrue:[ ^ self ].
	aMethod isInstalled ifFalse: [ ^ self ].
	
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
		self class addLog: self ]
]
