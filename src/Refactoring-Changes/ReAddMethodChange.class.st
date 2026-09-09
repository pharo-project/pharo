"
I am a refactory change for a method change. I know a bout the 
source selector and protocol of the new or modified method. 

The controller instance variable is not directly use by me but needed for executing the change by compiling this
method, the controller is what we pass to the compiler, as the object to get notified by the compiler about the compilation result.

"
Class {
	#name : 'ReAddMethodChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'source',
		'selector',
		'protocols',
		'definedSelector'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReAddMethodChange class >> compile: aString in: aClass [

	^ self new class: aClass source: aString
]

{ #category : 'instance creation' }
ReAddMethodChange class >> compile: aString in: aBehavior classified: aProtocol [

	^ self new class: aBehavior protocol: aProtocol source: aString
]

{ #category : 'comparing' }
ReAddMethodChange >> = anAddMethodChange [

	super = anAddMethodChange ifFalse: [ ^ false ].
	^ self parseTree = anAddMethodChange parseTree
]

{ #category : 'converting' }
ReAddMethodChange >> asUndoOperation [

	^ ( self changeClass includesSelector: self selector )
		ifTrue: [ | oldProtocol |

			oldProtocol := RBBrowserEnvironment new whichProtocolIncludes: self selector in: self changeClass.
			oldProtocol ifNil: [ oldProtocol := #accessing ].
			changeFactory
				addMethodSource: ( self methodSourceFor: self selector )
				in: self changeClass
				classified: oldProtocol
			]
		ifFalse: [ changeFactory removeMethod: selector from: self changeClass ]
]

{ #category : 'accessing' }
ReAddMethodChange >> changeForClass: aClassName selector: aSelector [

	^ (isMeta not and: [
		   self selector = aSelector and: [ className = aClassName ] ])
		  ifTrue: [ self ]
		  ifFalse: [ nil ]
]

{ #category : 'accessing' }
ReAddMethodChange >> changeForMetaclass: aClassName selector: aSelector [

	^ (isMeta and: [
		   self selector = aSelector and: [ className = aClassName ] ])
		  ifTrue: [ self ]
		  ifFalse: [ nil ]
]

{ #category : 'printing' }
ReAddMethodChange >> changeString [

	^ self displayClassName , '>>' , self selector
]

{ #category : 'initialization' }
ReAddMethodChange >> class: aClass protocol: aProtocol source: aString [

	self changeClass: aClass.
	self protocols: aProtocol.
	source := aString
]

{ #category : 'initialization' }
ReAddMethodChange >> class: aClass source: aString [

	self changeClass: aClass.
	source := aString.
	self protocols: (RBBrowserEnvironment new whichProtocolIncludes: self selector in: aClass)
]

{ #category : 'accessing' }
ReAddMethodChange >> definedSelector [

	^ definedSelector
]

{ #category : 'private' }
ReAddMethodChange >> generateChanges [

	definedSelector := self changeClass compile: self source classified: self protocol
]

{ #category : 'comparing' }
ReAddMethodChange >> hash [

	^ self parseTree hash
]

{ #category : 'accessing' }
ReAddMethodChange >> oldVersionTextToDisplay [
	| class |

	class := (onSystemEnvironment
					classNamed: className)
					ifNil: [ ^ super oldVersionTextToDisplay ].

	isMeta ifTrue: [ class := class classSide ].

	^ class methodDict
			at: self selector
			ifPresent: [:method | method sourceCode ]
			ifAbsent: [ super oldVersionTextToDisplay ]
]

{ #category : 'private' }
ReAddMethodChange >> parseTree [
	| tree |
	tree := self parserClass
		  parseMethod: self source
		  onError: [ :str :pos | ^ nil ].
	tree ifNotNil: [ tree doSemanticAnalysis ].
	^ tree
]

{ #category : 'printing' }
ReAddMethodChange >> printOn: aStream [

	aStream
		nextPut: $!;
		nextPutAll: self displayClassName;
		nextPutAll: ' methodsFor: ''';
		nextPutAll: self protocol;
		nextPutAll: ''' stamp: ';
		print: self changeStamp;
		cr
]

{ #category : 'accessing' }
ReAddMethodChange >> protocol [

	^ self protocols first
]

{ #category : 'accessing' }
ReAddMethodChange >> protocols [

	^ protocols
]

{ #category : 'initialization' }
ReAddMethodChange >> protocols: aCollectionOrProtocol [

	protocols := (aCollectionOrProtocol isCollection and: [ aCollectionOrProtocol isString not])
		             ifTrue: [ aCollectionOrProtocol ]
		             ifFalse: [  { aCollectionOrProtocol } ]
]

{ #category : 'accessing' }
ReAddMethodChange >> selector [

	selector ifNil: [
		selector := self parserClass parseMethodPattern: source.
		selector ifNil: [ selector := #unknown ] ].
	^ selector
]

{ #category : 'accessing' }
ReAddMethodChange >> source [

	^ source
]

{ #category : 'accessing' }
ReAddMethodChange >> textToDisplay [

	^ self source
]
