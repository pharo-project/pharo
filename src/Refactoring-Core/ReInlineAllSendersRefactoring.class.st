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
	#name : 'ReInlineAllSendersRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'numberReplaced',
		'numberNotReplaced',
		'pattern',
		'selectorString'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
ReInlineAllSendersRefactoring class >> model: aRBSmalltalk sendersOf: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		sendersOf: aSelector in: aClass;
		yourself
]

{ #category : 'instance creation' }
ReInlineAllSendersRefactoring class >> sendersOf: aSelector in: aClass [
	^ self new
		sendersOf: aSelector
		in: aClass
]

{ #category : 'preconditions' }
ReInlineAllSendersRefactoring >> applicabilityPreconditions [

	^ { (ReDefinesSelectorsCondition new definesSelectors: (Array with: selector) in: class) }
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> checkInlinedMethods [
	numberReplaced = 0
		ifTrue: [ InformativeNotification signal:  'Could not find any senders to inline into']
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> initializePatternFor: aClass [
	pattern := nil.
	(aClass = class or: [(aClass directlyDefinesMethod: selector) not]) ifTrue: [
		pattern := 'self ' , self selectorString.
		^ self].
	((aClass directlyDefinesMethod: selector) and:
		[ (aClass firstSuperclassRedefines: selector) = class]) ifTrue: [
		pattern := 'super ' , self selectorString ]
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> inlineMessagesInClass: aClass andSelector: aSelector [
	| messagesToInline previousCountOfMessages rbMethod |
	previousCountOfMessages := 4294967295.	"Some really large number > # of initial self sends."
	rbMethod := aClass parseTreeForSelector: aSelector.
	rbMethod ifNil: [ ^ self ].
	[ messagesToInline := self numberOfSelfSendsIn: (aClass parseTreeForSelector: aSelector).
	messagesToInline > 0 and: [previousCountOfMessages > messagesToInline]]
			whileTrue:
				[| node |
				previousCountOfMessages := messagesToInline.
				node := self selfSendIn: (aClass parseTreeForSelector: aSelector).
				self onError:
						[self generateChangesFor: ((RBInlineMethodRefactoring
									model: self model
									inline: node sourceInterval
									inMethod: aSelector
									forClass: aClass) checkOverridden: false).
						numberReplaced := numberReplaced + 1]
					do: [ ]].
	numberNotReplaced := numberNotReplaced + messagesToInline
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> inlineSelfSends [
	class withAllSubclasses do:
		[:each |
			| selectors |
			self initializePatternFor: each.
			pattern ifNotNil: [
			selectors := each localSelectors.
			class = each ifTrue: [ selectors remove: selector ifAbsent: []].
			selectors do: [:sel | self inlineMessagesInClass: each andSelector: sel]]]
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> messagePattern [
	^'self ' , (self buildSelectorString: selector)
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> numberOfSelfSendsIn: aParseTree [
	| search |
	search := self parseTreeSearcher.
	search
		matches: pattern
		do: [ :aNode :answer | answer + 1 ].
	^ search executeTree: aParseTree initialAnswer: 0
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> privateTransform [
	self
		inlineSelfSends;
		removeMethod;
		checkInlinedMethods
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> removeMethod [
	self onError:
			[self generateChangesFor: (RBRemoveMethodRefactoring
						model: self model
						selector: selector
						from: class)]
		do: []
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> selectorString [
	^selectorString ifNil: [ selectorString := self buildSelectorString: selector ]
]

{ #category : 'transforming' }
ReInlineAllSendersRefactoring >> selfSendIn: aTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: pattern
		do: [ :aNode :answer | ^ aNode ].
	^ searcher executeTree: aTree initialAnswer: nil
]

{ #category : 'initialization' }
ReInlineAllSendersRefactoring >> sendersOf: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	numberReplaced := numberNotReplaced := 0
]
