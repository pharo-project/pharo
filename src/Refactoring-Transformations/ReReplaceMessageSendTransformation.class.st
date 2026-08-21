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
	#name : 'ReReplaceMessageSendTransformation',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'replaceInAllClasses',
		'oldSelector',
		'newSelector',
		'permutation',
		'newArgs'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'displaying' }
ReReplaceMessageSendTransformation class >> basicMenuItemString [

	^ 'Replace senders'
]

{ #category : 'displaying' }
ReReplaceMessageSendTransformation class >> isTransformation [ 

	^ true
]

{ #category : 'instance creation' }
ReReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : 'instance creation' }
ReReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
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
ReReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean newArgs: anArgsCollection [
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
ReReplaceMessageSendTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap newArgs: anArgsCollection [
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
ReReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : 'instance creation' }
ReReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		inAllClasses: aBoolean 
]

{ #category : 'instance creation' }
ReReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean newArgs: anArgsCollection [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		inAllClasses: aBoolean
		newArgs: anArgsCollection 
]

{ #category : 'instance creation' }
ReReplaceMessageSendTransformation class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap newArgs: anArgsCollection [
	^self new
		replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		newArgs: anArgsCollection 
]

{ #category : 'preconditions' }
ReReplaceMessageSendTransformation >> applicabilityPreconditions [

	^ {
		  (ReArgumentsAreNotTheSameSizeCondition new oldSelector: oldSelector newSelector: newSelector newArgs: newArgs).
		  (ReDefinesSelectorsCondition new definesSelectors: (Array with: oldSelector with: newSelector) in: class) }
]

{ #category : 'support' }
ReReplaceMessageSendTransformation >> convertAllReferencesTo: aSymbol of: classes using: searchReplacer [
	(self model allReferencesTo: aSymbol in: classes)
		do:
			[:method |
			self
				convertMethod: method selector
				for: method modelClass
				using: searchReplacer]
]

{ #category : 'support' }
ReReplaceMessageSendTransformation >> convertAllReferencesTo: aSymbol using: searchReplacer [

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
ReReplaceMessageSendTransformation >> newArgs [

	^ newArgs ifNil: [ newArgs := {  } ]
]

{ #category : 'accessing' }
ReReplaceMessageSendTransformation >> newArgs: anArgsCollection [

	newArgs := anArgsCollection
]

{ #category : 'private' }
ReReplaceMessageSendTransformation >> newSelectorString [

	^ self buildSelectorString: newSelector withPermuteMap: permutation andNewArguments: newArgs
]

{ #category : 'parsing' }
ReReplaceMessageSendTransformation >> parseTreeRewriter [
	| rewriteRule oldString newString |
	
	oldString := self buildSelectorString: oldSelector.
	newString := self newSelectorString.
	
	rewriteRule := self parseTreeRewriterInstance.
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule
]

{ #category : 'parsing' }
ReReplaceMessageSendTransformation >> parseTreeRewriterInstance [

	^ self parseTreeRewriterClass
		  replaceLiteral: oldSelector
		  with: newSelector
]

{ #category : 'transforming' }
ReReplaceMessageSendTransformation >> privateTransform [
	self replaceInAllClasses
		ifTrue: [ self replaceMessageSends ]
		ifFalse: [ self replaceMessageSendsIn: {class} ]
]

{ #category : 'initialization' }
ReReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap [

	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap.
	newArgs := OrderedCollection new.
]

{ #category : 'initialization' }
ReReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap inAllClasses: aBoolean [

	self replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap.
	replaceInAllClasses := aBoolean
]

{ #category : 'initialization' }
ReReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap inAllClasses: aBoolean newArgs: anArgsCollection [

	self replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap newArgs: anArgsCollection.
	replaceInAllClasses := aBoolean
]

{ #category : 'initialization' }
ReReplaceMessageSendTransformation >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap newArgs: anArgsCollection [

	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap.
	newArgs := anArgsCollection 
]

{ #category : 'accessing' }
ReReplaceMessageSendTransformation >> replaceInAllClasses [
	^ replaceInAllClasses ifNil: [ replaceInAllClasses := false ]
]

{ #category : 'transforming' }
ReReplaceMessageSendTransformation >> replaceMessageSends [
	self convertAllReferencesTo: oldSelector using: self parseTreeRewriter
]

{ #category : 'transforming' }
ReReplaceMessageSendTransformation >> replaceMessageSendsIn: classes [
	self convertAllReferencesTo: oldSelector of: classes using: self parseTreeRewriter
]
