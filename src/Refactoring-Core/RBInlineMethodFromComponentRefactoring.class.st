"
I am a refactoring for replacing method calls by the method implementation.

Just like `RBInlineMethodRefactoring`,  I replace a message send by the implementation of that  message , but you can provide the component
where this implementation is taken from or choose one if there are move than one implementors.
If the method implementation has some direct variable references, accessor for this variable are created (just as by the generate accessor refactoring).
"
Class {
	#name : 'RBInlineMethodFromComponentRefactoring',
	#superclass : 'RBInlineMethodRefactoring',
	#category : 'Refactoring-Core-Refactorings-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings-Unused'
}

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> abstractVariableReferences [
	| refactoring |
	refactoring := RBAbstractVariablesRefactoring
				model: self model
				abstractVariablesIn: inlineParseTree
				from: self classOfTheMethodToInline
				toAll: (Array with: class).
	self generateChangesFor: refactoring.
	inlineParseTree := refactoring parseTree
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> addArgumentToSelector: aSymbol [
	^aSymbol isInfix
		ifTrue: [#value:value:]
		ifFalse: [(aSymbol , 'value:') asSymbol]
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> addSelfReferenceToInlineParseTree [
	| variableName rewriter newArguments |

	variableName := self newNameForSelf.
	rewriter := self parseTreeRewriterClass rename: 'self' to: variableName.
	(rewriter executeTree: inlineParseTree)
		ifTrue: [inlineParseTree := rewriter tree].
	newArguments := inlineParseTree arguments asOrderedCollection.
	newArguments addFirst: (OCVariableNode named: variableName).
	inlineParseTree
		renameSelector: (self addArgumentToSelector: inlineParseTree selector)
		andArguments: newArguments.
	sourceMessage receiver replaceWith: (OCVariableNode named: variableName)
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> addSelfReferenceToSourceMessage [
	| newArguments |

	newArguments := sourceMessage arguments asOrderedCollection.
	newArguments addFirst: sourceMessage receiver copy.
	sourceMessage renameSelector: (self addArgumentToSelector: sourceMessage selector) andArguments: newArguments
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> checkSelectedMessage [

	sourceParseTree := class parseTreeForSelector: sourceSelector.
	sourceParseTree ifNil: [ self refactoringError: 'Could not parse sources' ].
	sourceMessage := sourceParseTree whichNodeIsContainedBy: sourceInterval.
	sourceMessage
		ifNil: [ self refactoringError: 'The selection doesn''t appear to be a message send' ].
	sourceMessage isCascade
		ifTrue: [ sourceMessage := sourceMessage messages last ].
	sourceMessage isMessage
		ifFalse: [ self refactoringError: 'The selection doesn''t appear to be a message send' ]
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> checkSuperMessages [
	inlineParseTree superMessages isEmpty
		ifFalse:
			[self
				refactoringError: 'Cannot inline method since it sends a super message']
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> classOfTheMethodToInline [

	| imps |

	classOfTheMethodToInline ifNotNil: [ ^ classOfTheMethodToInline ].
	imps := ( self model allImplementorsOf: self inlineSelector ) asOrderedCollection.
	imps size = 1
		ifTrue: [ ^ classOfTheMethodToInline := imps first ].
	imps ifEmpty: [ self refactoringError: 'Nobody defines a method named ' , self inlineSelector ].
	classOfTheMethodToInline := self requestImplementorToInline: imps.
	classOfTheMethodToInline ifNil: [ self refactoringError: 'No implementor selected' ].
	^ classOfTheMethodToInline
]

{ #category : 'testing' }
RBInlineMethodFromComponentRefactoring >> isOverridden [
	| selector|
	selector := self inlineSelector.
	self classOfTheMethodToInline allSubclassesDo: [:each |
		(each directlyDefinesMethod: selector)
			ifTrue: [ ^ true ]].
	^ false
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> nameOfTheClassOfTheMethodToInline [

	^ self classOfTheMethodToInline instanceSide name
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> newNameForSelf [

	| variableName index originalName nonMetaClass |
	nonMetaClass := self nameOfTheClassOfTheMethodToInline.
	variableName := originalName := (nonMetaClass first isVowel
		                                 ifTrue: [ 'an' ]
		                                 ifFalse: [ 'a' ])
	                                , nonMetaClass.
	index := 1.

	[
	variableName := self safeVariableNameBasedOn: variableName.
	inlineParseTree allDefinedVariables includes: variableName ]
		whileTrue: [
			variableName := originalName , index printString.
			index := index + 1 ].
	^ variableName
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> privateTransform [
	self abstractVariableReferences.
	self renameSelfReferences.
	super privateTransform
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> renameSelfReferences [
	self addSelfReferenceToSourceMessage.
	self addSelfReferenceToInlineParseTree
]

{ #category : 'transforming' }
RBInlineMethodFromComponentRefactoring >> safeVariableNameBasedOn: aString [
	"Creates an unused variable name containing aString"

	| baseString newString i allTempsInline allTempsSource |
	allTempsInline := inlineParseTree allTemporaryVariables.
	allTempsSource := sourceParseTree allDefinedVariables.
	baseString := aString copy.
	baseString at: 1 put: baseString first asLowercase.
	newString := baseString.
	i := 0.

	[(allTempsInline includes: newString)
		or: [ (allTempsSource includes: newString)
			or: [class definesInstanceVariable: newString]]]
			whileTrue:
				[i := i + 1.
				newString := baseString , i printString].
	^newString
]
