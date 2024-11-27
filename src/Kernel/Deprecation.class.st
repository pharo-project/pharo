"
This Warning is signalled by methods which are deprecated.

Idiom: Imagine I want to deprecate the message #foo.

```
foo
	^ 'foo'
```

I can replace it with:

```
foo
	self 
		deprecated:   'The method #foo was not good. Use Bar>>newFoo instead.'
		on:  'here add date'
		in:  'here add version'
		transformWith:   '`@receiver foo' -> '`@receiver newFoo'.	
	^self newFoo
	
```
	
The  transformWith:  part is optional. It allows to transform the deprecated method automatically when called.
If the transformation is defined, the Warning will not signalled.


Check the class side for configurations. You can raise, ignore, log and rewrite callers. 


"
Class {
	#name : 'Deprecation',
	#superclass : 'AutomaticRewriting',
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'logging' }
Deprecation class >> deprecationsWhile: aBlock [
	"returns a log of all deprecated methods seen while executing aBlock"
	| oldLog result |
	oldLog := log.
	log := Set new.
	aBlock value.
	result := log.
	oldLog ifNotNil: [oldLog addAll: result].
	log := oldLog.
	^result
]

{ #category : 'handling' }
Deprecation >> defaultAction [

	self class addLog: self.
	self showWarning ifTrue: [ self logTranscript ].
	self raiseWarning ifTrue: [ super defaultAction ].
	self shouldTransform ifTrue: [ self transform ]
]

{ #category : 'private' }
Deprecation >> logTranscript [
	DeprecationPerformedNotification signal: self messageText
]

{ #category : 'accessing' }
Deprecation >> messageText [
	^String streamContents: [ :str |
		self shouldTransform ifTrue: [
			str nextPutAll:  'Automatic deprecation caller rewrite: '].
		str
			nextPutAll: 'The method ';
			nextPutAll: self deprecatedMethodName;
			nextPutAll: ' called from ';
			nextPutAll: self sendingMethodName;
			nextPutAll: ' has been deprecated. ';
		 	nextPutAll: explanationString]
]

{ #category : 'handling' }
Deprecation >> transform [

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
