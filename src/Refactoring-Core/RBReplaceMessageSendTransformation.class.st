"
I'm a transformation replaces one message send by another one.
As such I cannot garantee anything about behavior preservation.

The new method's name can have a different number of arguments than the original method, 
if it has more arguments a list of initializers will be needed for them.

All senders of this method are changed by the other.

### Example

```
(RBReplaceMessageSendTransformation
	model: model
	replaceMethod: #anInstVar:
	in: RBBasicLintRuleTestData
	to: #newResultClass: 
	permutation: (1 to: 1)
	inAllClasses: true) execute
```
"
Class {
	#name : #RBReplaceMessageSendTransformation,
	#superclass : #RBChangeMethodNameRefactoring,
	#instVars : [
		#replaceInAllClasses,
		#initializers
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #displaying }
RBReplaceMessageSendTransformation class >> basicMenuItemString [

	^ 'Replace senders'
]

{ #category : #accessing }
RBReplaceMessageSendTransformation class >> kind [

	^ 'Transformation'
]

{ #category : #'instance creation' }
RBReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : #'instance creation' }
RBReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap
			inAllClasses: aBoolean;
		yourself
]

{ #category : #'instance creation' }
RBReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : #'instance creation' }
RBReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		inAllClasses: aBoolean
]

{ #category : #accessing }
RBReplaceMessageSendTransformation >> initializers [

	^ initializers ifNil: [ initializers := {  } ]
]

{ #category : #accessing }
RBReplaceMessageSendTransformation >> initializers: anObject [

	initializers := anObject
]

{ #category : #private }
RBReplaceMessageSendTransformation >> newSelectorString [

	| stream keywords |
	stream := WriteStream on: String new.
	permutation ifEmpty: [ stream nextPutAll: newSelector ].
	keywords := newSelector keywords.
	permutation
		doWithIndex:
			[:each :index |
			stream nextPutAll: (keywords at: index).
			each < 0
				ifTrue:
					[stream
						nextPut: $(;
						nextPutAll: (initializers at: each abs);
						nextPut: $)]
				ifFalse:
					[stream
						nextPutAll: ' ``@arg';
						nextPutAll: each printString].
			stream nextPut: $ ].
	^stream contents
]

{ #category : #transforming }
RBReplaceMessageSendTransformation >> parseTreeRewriter [

	| rewriteRule oldString newString |
	rewriteRule := self parseTreeRewriterClass new.
	oldString := self buildSelectorString: oldSelector.
	newString := self newSelectorString.
	rewriteRule
		replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^ rewriteRule
]

{ #category : #preconditions }
RBReplaceMessageSendTransformation >> preconditions [

	| conditions |
	conditions := (RBCondition withBlock: [
		               oldSelector numArgs < newSelector numArgs ifTrue: [
			               oldSelector numArgs + self initializers size
			               = newSelector numArgs ifFalse: [
				               self refactoringFailure:
					               'You don''t have the necessary initializers to replace senders.' ] ].
		               true ])
	              & (RBCondition definesSelector: oldSelector in: class)
	              & (RBCondition definesSelector: newSelector in: class).
	"This is unclear that the targeting method should already be defined."

	^ conditions
]

{ #category : #initialization }
RBReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap [

	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap
]

{ #category : #initialization }
RBReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap inAllClasses: aBoolean [

	self replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap.
	replaceInAllClasses := aBoolean
]

{ #category : #accessing }
RBReplaceMessageSendTransformation >> replaceInAllClasses [
	^ replaceInAllClasses ifNil: [ replaceInAllClasses := false ]
]

{ #category : #transforming }
RBReplaceMessageSendTransformation >> transform [
	self replaceInAllClasses
		ifTrue: [ self renameMessageSends ]
		ifFalse: [ self renameMessageSendsIn: {class} ]
]
