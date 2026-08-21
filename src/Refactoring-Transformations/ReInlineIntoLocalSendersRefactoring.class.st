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
	#name : 'ReInlineIntoLocalSendersRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'numberReplaced',
		'numberNotReplaced',
		'pattern',
		'selectorString'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
ReInlineIntoLocalSendersRefactoring class >> model: aRBSmalltalk sendersOf: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		sendersOf: aSelector in: aClass;
		yourself
]

{ #category : 'instance creation' }
ReInlineIntoLocalSendersRefactoring class >> sendersOf: aSelector in: aClass [
	^ self new
		sendersOf: aSelector
		in: aClass
]

{ #category : 'preconditions' }
ReInlineIntoLocalSendersRefactoring >> applicabilityPreconditions [

	^ { (RBCondition definesSelector: selector in: class) }
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> checkInlinedMethods [
	numberReplaced = 0
		ifTrue: [ InformativeNotification signal:  'Could not find any senders to inline into']
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> initializePatternFor: aClass [
	pattern := nil.
	(aClass = class or: [(aClass directlyDefinesMethod: selector) not]) ifTrue: [
		pattern := 'self ' , self selectorString.
		^ self].
	((aClass directlyDefinesMethod: selector) and:
		[ (aClass firstSuperclassRedefines: selector) = class]) ifTrue: [
		pattern := 'super ' , self selectorString ]
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> inlineMessagesInClass: aClass andSelector: aSelector [
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
ReInlineIntoLocalSendersRefactoring >> inlineSelfSends [
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
ReInlineIntoLocalSendersRefactoring >> messagePattern [
	^'self ' , (self buildSelectorString: selector)
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> numberOfSelfSendsIn: aParseTree [
	| search |
	search := self parseTreeSearcher.
	search
		matches: pattern
		do: [ :aNode :answer | answer + 1 ].
	^ search executeTree: aParseTree initialAnswer: 0
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> privateTransform [
	self
		inlineSelfSends;
		removeMethod;
		checkInlinedMethods
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> removeMethod [
	self onError:
			[self generateChangesFor: (RBRemoveMethodRefactoring
						model: self model
						selector: selector
						from: class)]
		do: []
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> selectorString [
	^selectorString ifNil: [ selectorString := self buildSelectorString: selector ]
]

{ #category : 'transforming' }
ReInlineIntoLocalSendersRefactoring >> selfSendIn: aTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: pattern
		do: [ :aNode :answer | ^ aNode ].
	^ searcher executeTree: aTree initialAnswer: nil
]

{ #category : 'initialization' }
ReInlineIntoLocalSendersRefactoring >> sendersOf: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	numberReplaced := numberNotReplaced := 0
]
