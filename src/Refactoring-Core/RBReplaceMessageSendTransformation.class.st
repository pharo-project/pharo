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
	#name : 'RBReplaceMessageSendTransformation',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'replaceInAllClasses',
		'oldSelector',
		'newSelector',
		'permutation',
		'newArgs'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'displaying' }
RBReplaceMessageSendTransformation class >> basicMenuItemString [

	^ 'Replace senders'
]

{ #category : 'displaying' }
RBReplaceMessageSendTransformation class >> isTransformation [ 

	^ true
]

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : 'instance creation' }
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

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean newArgs: anArgsCollection [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap
			inAllClasses: aBoolean
			newArgs: anArgsCollection;
		yourself
]

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap newArgs: anArgsCollection [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap
			newArgs: anArgsCollection;
		yourself
]

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		inAllClasses: aBoolean 
]

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean newArgs: anArgsCollection [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		inAllClasses: aBoolean
		newArgs: anArgsCollection 
]

{ #category : 'instance creation' }
RBReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap newArgs: anArgsCollection [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		newArgs: anArgsCollection 
]

{ #category : 'preconditions' }
RBReplaceMessageSendTransformation >> applicabilityPreconditions [

	| conditions |
	conditions := (RBCondition withBlock: [
		               oldSelector numArgs < newSelector numArgs ifTrue: [
			               oldSelector numArgs + self newArgs size
			               = newSelector numArgs ifFalse: [
				               self refactoringError:
					               'You don''t have the necessary initializers to replace senders.' ] ].
		               true ])
	              & (RBCondition definesSelector: oldSelector in: class)
	              & (RBCondition definesSelector: newSelector in: class).
	"This is unclear that the targeting method should already be defined."

	^ conditions
]

{ #category : 'support' }
RBReplaceMessageSendTransformation >> convertAllReferencesTo: aSymbol of: classes using: searchReplacer [
	(self model allReferencesTo: aSymbol in: classes)
		do:
			[:method |
			self
				convertMethod: method selector
				for: method modelClass
				using: searchReplacer]
]

{ #category : 'support' }
RBReplaceMessageSendTransformation >> convertAllReferencesTo: aSymbol using: searchReplacer [

	(self model allReferencesTo: aSymbol)
		reject: [ :rbMethod | "We reject methods from trait to not recompile them in the class and duplicate the code."
			rbMethod compiledMethod
				ifNil: [ false ]
				ifNotNil: [ :m | m isFromTrait ] ]
		thenDo: [ :method |
			self
				convertMethod: method selector
				for: method modelClass
				using: searchReplacer ]
]

{ #category : 'accessing' }
RBReplaceMessageSendTransformation >> newArgs [

	^ newArgs ifNil: [ newArgs := {  } ]
]

{ #category : 'accessing' }
RBReplaceMessageSendTransformation >> newArgs: anArgsCollection [

	newArgs := anArgsCollection
]

{ #category : 'private' }
RBReplaceMessageSendTransformation >> newSelectorString [

	^ self buildSelectorString: newSelector withPermuteMap: permutation andNewArguments: newArgs
]

{ #category : 'parsing' }
RBReplaceMessageSendTransformation >> parseTreeRewriter [
	| rewriteRule oldString newString |
	
	oldString := self buildSelectorString: oldSelector.
	newString := self newSelectorString.
	
	rewriteRule := self parseTreeRewriterInstance.
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule
]

{ #category : 'parsing' }
RBReplaceMessageSendTransformation >> parseTreeRewriterInstance [

	^ self parseTreeRewriterClass
		  replaceLiteral: oldSelector
		  with: newSelector
]

{ #category : 'transforming' }
RBReplaceMessageSendTransformation >> privateTransform [
	self replaceInAllClasses
		ifTrue: [ self replaceMessageSends ]
		ifFalse: [ self replaceMessageSendsIn: {class} ]
]

{ #category : 'initialization' }
RBReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap [

	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap.
	newArgs := OrderedCollection new.
]

{ #category : 'initialization' }
RBReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap inAllClasses: aBoolean [

	self replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap.
	replaceInAllClasses := aBoolean
]

{ #category : 'initialization' }
RBReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap inAllClasses: aBoolean newArgs: anArgsCollection [

	self replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap newArgs: anArgsCollection.
	replaceInAllClasses := aBoolean
]

{ #category : 'initialization' }
RBReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap newArgs: anArgsCollection [

	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap.
	newArgs := anArgsCollection 
]

{ #category : 'accessing' }
RBReplaceMessageSendTransformation >> replaceInAllClasses [
	^ replaceInAllClasses ifNil: [ replaceInAllClasses := false ]
]

{ #category : 'transforming' }
RBReplaceMessageSendTransformation >> replaceMessageSends [
	self convertAllReferencesTo: oldSelector using: self parseTreeRewriter
]

{ #category : 'transforming' }
RBReplaceMessageSendTransformation >> replaceMessageSendsIn: classes [
	self convertAllReferencesTo: oldSelector of: classes using: self parseTreeRewriter
]
