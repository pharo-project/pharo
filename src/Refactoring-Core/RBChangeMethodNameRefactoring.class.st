"
I am an abstract base class for refactorings changing a method name.

Doing a method rename involves:
- rename implementors
- rename message sends and
- remove renamed implementors.

I implement the above precedures and provide helper functions for finding and renaming references.
Every concrete subclass has to add its own precondition (see `myPrecondition`).

"
Class {
	#name : 'RBChangeMethodNameRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'newSelector',
		'oldSelector',
		'permutation',
		'implementors',
		'renameMap'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'testing' }
RBChangeMethodNameRefactoring class >> isAbstract [

	^ self == RBChangeMethodNameRefactoring
]

{ #category : 'preconditions' }
RBChangeMethodNameRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: oldSelector in: class).
		  self isValidMethodNamePrecondition }
]

{ #category : 'preconditions' }
RBChangeMethodNameRefactoring >> breakingChangePreconditions [
	"This refactoring only preserves behavior if all implementors are renamed."

	^ { self doesNotOverrideExistingMethodPrecondition }
]

{ #category : 'preconditions' }
RBChangeMethodNameRefactoring >> doesNotOverrideExistingMethodPrecondition [
	"Check that the new selector is not already defined in superclasses"
	
	^ self implementors
		inject: RBCondition true 
		into: [ :condition :each |
			condition & (RBCondition hierarchyOf: each canUnderstand: newSelector) not ]
]

{ #category : 'testing' }
RBChangeMethodNameRefactoring >> hasPermutedArguments [
	oldSelector numArgs = newSelector numArgs ifFalse: [^true].
	1 to: oldSelector numArgs
		do: [:i | (permutation at: i) = i ifFalse: [^true]].
	^false
]

{ #category : 'private' }
RBChangeMethodNameRefactoring >> implementors [

	implementors ifNil: [ implementors := self model allImplementorsOf: oldSelector ].
	^ implementors
]

{ #category : 'testing' }
RBChangeMethodNameRefactoring >> implementorsCanBePrimitives [
	^false
]

{ #category : 'preconditions' }
RBChangeMethodNameRefactoring >> isValidMethodNamePrecondition [
	^ (RBCondition isValidMethodName: newSelector for: class)

	
]

{ #category : 'private' }
RBChangeMethodNameRefactoring >> modifyImplementorParseTree: parseTree in: aClass [
	| oldArgs |
	oldArgs := parseTree arguments.
	parseTree renameSelector: newSelector andArguments: (permutation collect: [:each | oldArgs at: each])
]

{ #category : 'instance creation' }
RBChangeMethodNameRefactoring >> newArgs [ 

	^ OrderedCollection new
]

{ #category : 'accessing' }
RBChangeMethodNameRefactoring >> newSelector [
	^ newSelector
]

{ #category : 'accessing' }
RBChangeMethodNameRefactoring >> newSelector: aSymbol [
	newSelector := aSymbol
]

{ #category : 'accessing' }
RBChangeMethodNameRefactoring >> permutation: anObject [

	permutation := anObject
]

{ #category : 'preconditions' }
RBChangeMethodNameRefactoring >> preconditions [
	"This refactoring only preserves behavior if all implementors are renamed."

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBChangeMethodNameRefactoring >> privateTransform [
	self renameImplementors.
	self replaceMessageSends.
	self removeRenamedImplementors
]

{ #category : 'transforming' }
RBChangeMethodNameRefactoring >> removeRenamedImplementors [
	oldSelector = newSelector
		ifTrue: [ ^ self ].
	self implementors
		do: [ :each | 
			self generateChangesFor:
				(RBRemoveMethodTransformation
					selector: oldSelector
					from: each) ]
]

{ #category : 'action' }
RBChangeMethodNameRefactoring >> renameArgumentsIn: parseTree [
	self renameMap do: [ :arg |
		(self parseTreeRewriterClass rename: arg name to: arg newName) executeTree: parseTree
	]
]

{ #category : 'transforming' }
RBChangeMethodNameRefactoring >> renameImplementors [

	self implementors
		do: [ :each |
			| parseTree |

			parseTree := each parseTreeForSelector: oldSelector.
			parseTree ifNil: [ self refactoringError: 'Could not parse source code.' ].
			self implementorsCanBePrimitives
				ifFalse: [ parseTree isPrimitive
						ifTrue: [ self
								refactoringError:
									( '<1p>''s implementation of #<2s> is a primitive' expandMacrosWith: each with: oldSelector )
							]
					].
			self modifyImplementorParseTree: parseTree in: each.
			( each methodFor: oldSelector ) compileTree: parseTree
			]
]

{ #category : 'accessing' }
RBChangeMethodNameRefactoring >> renameMap [
	^renameMap ifNil: [ renameMap := { } ]
]

{ #category : 'accessing' }
RBChangeMethodNameRefactoring >> renameMap: aColl [
	renameMap := aColl
]

{ #category : 'initialization' }
RBChangeMethodNameRefactoring >> renameMethod: aSelector in: aClass [
	"Watch out this is for partial configuration of the refactoring"

	oldSelector := aSelector asSymbol.
	class := self classObjectFor: aClass.
	
]

{ #category : 'initialization' }
RBChangeMethodNameRefactoring >> renameMethod: aSelector in: aClass to: newSel permutation: aMap [
	
	self renameMethod: aSelector asSymbol in: aClass.
	newSelector := newSel asSymbol.
	permutation := aMap
]

{ #category : 'transforming' }
RBChangeMethodNameRefactoring >> replaceMessageSends [
	| className |
	
	className :=  class name, (class isMeta 
		ifTrue: [ ' class' ]
		ifFalse: [ '' ]).
	
	self generateChangesFor: (RBReplaceMessageSendTransformation
			 model: self model
			 replaceMethod: oldSelector
			 in: (self model classNamed: className)
			 to: newSelector
			 permutation: permutation
			 inAllClasses: true
			 newArgs: self newArgs)
]
