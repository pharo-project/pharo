"
I am a refactoring for inlining code of this method.

The call to this method in all other methods of this class is replaced by its implementation. The method itself will be removed.

For example, a method 

```
foo
	^ 'text'.
```	
is called in

```
baz
	| a |
	a := self foo.
	^ self foo.
```	
inlining in all senders replaces the call to method foo, with its code:

```
baz
	| a |
	a := 'text'.
	^ 'text'.
```

"
Class {
	#name : #RBInlineAllSendersRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'numberReplaced',
		'numberNotReplaced',
		'pattern',
		'selectorString'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBInlineAllSendersRefactoring class >> model: aRBSmalltalk sendersOf: aSelector in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		sendersOf: aSelector in: aClass;
		yourself
]

{ #category : #'instance creation' }
RBInlineAllSendersRefactoring class >> sendersOf: aSelector in: aClass [ 
	^self new sendersOf: aSelector in: aClass
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> checkInlinedMethods [
	numberReplaced = 0 
		ifTrue: [self inform:  'Could not find any senders to inline into']
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> initializePatternFor: aClass [
	pattern := nil.
	(aClass = class or: [(aClass directlyDefinesMethod: selector) not]) ifTrue: [
		pattern := 'self ' , self selectorString.
		^ self].
	((aClass directlyDefinesMethod: selector) and: 
		[ (aClass firstSuperclassRedefines: selector) = class]) ifTrue: [ 
		pattern := 'super ' , self selectorString ].
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> inlineMessagesInClass: aClass andSelector: aSelector [ 
	| messagesToInline previousCountOfMessages rbMethod |
	previousCountOfMessages := 4294967295.	"Some really large number > # of initial self sends."
	rbMethod := aClass parseTreeFor: aSelector.
	rbMethod ifNil: [ ^ self ].
	[ messagesToInline := self numberOfSelfSendsIn: (aClass parseTreeFor: aSelector).
	messagesToInline > 0 and: [previousCountOfMessages > messagesToInline]] 
			whileTrue: 
				[| node |
				previousCountOfMessages := messagesToInline.
				node := self selfSendIn: (aClass parseTreeFor: aSelector).
				self onError: 
						[self performCompositeRefactoring: ((RBInlineMethodRefactoring
									model: self model
									inline: node sourceInterval
									inMethod: aSelector
									forClass: aClass) checkOverridden: false).
						numberReplaced := numberReplaced + 1]
					do: [ :e | ]].
	numberNotReplaced := numberNotReplaced + messagesToInline
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> inlineSelfSends [
	class withAllSubclasses do: 
		[:each | 
			| selectors |
			self initializePatternFor: each.
			pattern ifNotNil: [
			selectors := each localSelectors.
			class = each ifTrue: [ selectors remove: selector ifAbsent: []].
			selectors do: [:sel | self inlineMessagesInClass: each andSelector: sel]]]
	
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> messagePattern [
	^'self ' , (self buildSelectorString: selector)
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> numberOfSelfSendsIn: aParseTree [
	| search |
	search := self parseTreeSearcher.
	search
		matches: pattern
		do: [ :aNode :answer | answer + 1 ].
	^ search executeTree: aParseTree initialAnswer: 0
]

{ #category : #preconditions }
RBInlineAllSendersRefactoring >> preconditions [
	^RBCondition canUnderstand: selector in: class
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> removeMethod [
	self onError: 
			[self performCompositeRefactoring: (RBRemoveMethodRefactoring 
						model: self model
						removeMethods: (Array with: selector)
						from: class)]
		do: []
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> selectorString [
	^selectorString ifNil: [ selectorString := self buildSelectorString: selector ]
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> selfSendIn: aTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: pattern
		do: [ :aNode :answer | ^ aNode ].
	^ searcher executeTree: aTree initialAnswer: nil
]

{ #category : #initialization }
RBInlineAllSendersRefactoring >> sendersOf: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	numberReplaced := numberNotReplaced := 0
]

{ #category : #printing }
RBInlineAllSendersRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' sendersOf: #';
		nextPutAll: selector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBInlineAllSendersRefactoring >> transform [
	self
		inlineSelfSends;
		removeMethod;
		checkInlinedMethods
]
