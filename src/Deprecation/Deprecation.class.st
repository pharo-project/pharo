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
	#category : 'Deprecation-Exceptions',
	#package : 'Deprecation',
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
Deprecation >> compatibilityPeriod [
	"PHEP-0004: number of Pharo versions, the one that deprecates included, that keep a deprecated API working."

	^ self isKernel
		  ifTrue: [ 4 ]
		  ifFalse: [ 2 ]
]

{ #category : 'accessing' }
Deprecation >> currentMajorVersion [

	^ SystemVersion current major
]

{ #category : 'handling' }
Deprecation >> defaultAction [
    RecursionStopper during: [
        self class addLog: self.
        self showWarning ifTrue: [ self logTranscript ].
        self raiseWarning ifTrue: [ super defaultAction ].
        self shouldTransform ifTrue: [ self transform ]
    ]
]

{ #category : 'accessing' }
Deprecation >> deprecatedClass [

	^ self contextOfDeprecatedMethod homeMethod methodClass
]

{ #category : 'accessing' }
Deprecation >> deprecationMajorVersion [
	"Answer the major Pharo version given with #in:, such as #Pharo14 or 'pharo13', or nil when no Pharo version was given."

	| matcher |
	versionString ifNil: [ ^ nil ].
	matcher := '[Pp]haro\s*(\d+)' asRegex.
	(matcher search: versionString asString) ifFalse: [ ^ nil ].
	^ (matcher subexpression: 2) asNumber
]

{ #category : 'handling' }
Deprecation >> isAtEndOfCompatibilityPeriod [

	| deprecationVersion |
	deprecationVersion := self deprecationMajorVersion ifNil: [ ^ false ].
	^ self currentMajorVersion >= (deprecationVersion + self compatibilityPeriod - 1)
]

{ #category : 'accessing' }
Deprecation >> isKernel [
	"PHEP-0004: the deprecated API is kernel when the class that defines it belongs to the minimal image. The class is asked rather than the method, since deprecated methods move to DeprecatedN packages. Without the bootstrap baseline, assume kernel, which keeps the longer period."

	| baseline |
	baseline := self class environment at: #BaselineOfPharoBootstrap ifAbsent: [ ^ true ].
	^ baseline kernelPackageNames includes: self deprecatedClass instanceSide package name
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
Deprecation >> shouldTransform [
	"PHEP-0004: a deprecation rewrites its callers only when it states the Pharo version that deprecated it, and only in the last version of its compatibility period or later."

	^ super shouldTransform and: [ self isAtEndOfCompatibilityPeriod ]
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
