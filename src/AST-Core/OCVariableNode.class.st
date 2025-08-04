"
ASTVariableNode is an AST node that represents a variable (global, inst var, temp, etc.).

Although this is the basic class for the concrete variable types, this is not an abstract class and is actually used
by the parser for all variables that aren't special builtin types like self/super/thisContext. All other variables are
just ASTVariableNodes until the semantic analyser can deduce the type.

Instance Variables:
	name	<ASTValueToken>	the variable's name I represent
	nameStart <Integer>	the position where I was found at the source code

"
Class {
	#name : 'OCVariableNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'name',
		'variable',
		'start'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCVariableNode class >> identifierNamed: anIdentifierName at: aPosition [

	anIdentifierName = #self
		ifTrue: [ ^ self selfNode start: aPosition ].
	anIdentifierName = #thisContext
		ifTrue: [ ^ self thisContextNode start: aPosition ].
	anIdentifierName = #thisProcess
		ifTrue: [ ^ self thisProcessNode start: aPosition ].
	anIdentifierName = #super
		ifTrue: [ ^ self superNode start: aPosition ].
	^ self named: anIdentifierName start: aPosition
]

{ #category : 'instance creation' }
OCVariableNode class >> named: aName [
	^ self named: aName start: 0
]

{ #category : 'instance creation' }
OCVariableNode class >> named: aName start: aPosition [
	^self new
		named: aName start: aPosition;
		yourself
]

{ #category : 'instance creation' }
OCVariableNode class >> selfNode [
	^ self named: #self
]

{ #category : 'instance creation' }
OCVariableNode class >> superNode [
	^ self named: #super
]

{ #category : 'instance creation' }
OCVariableNode class >> thisContextNode [
	^ self named: #thisContext
]

{ #category : 'instance creation' }
OCVariableNode class >> thisProcessNode [
	^ self named: #thisProcess
]

{ #category : 'comparing' }
OCVariableNode >> = anObject [
	self == anObject ifTrue: [^true].
	((anObject isKindOf: self class) or: [self isKindOf: anObject class])
		ifFalse: [^false].
	^self name = anObject name
]

{ #category : 'visiting' }
OCVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ variable acceptVisitor: aProgramNodeVisitor node: self
]

{ #category : 'matching' }
OCVariableNode >> copyInContext: aDictionary [
	^ self class named: name
]

{ #category : 'comparing' }
OCVariableNode >> equalTo: anObject withMapping: aDictionary [
	"If we have two variables make sure that the dictionary associates both names."
	
	^ self class = anObject class 
		and: [ (aDictionary at: self name ifAbsentPut: [ anObject name ])
		  = anObject name ]
]

{ #category : 'testing' }
OCVariableNode >> hasIncompleteIdentifier [
	"check if there is any variable declared in my scope that starts with my name"

	"declared vars are never incomplete"
	variable isUndeclaredVariable ifFalse: [ ^false ].
	"for the others we have to search from the current scope"
	^self scope hasBindingThatBeginsWith: self name
]

{ #category : 'comparing' }
OCVariableNode >> hash [
	^self name hash
]

{ #category : 'initialization' }
OCVariableNode >> initialize [
	super initialize.
	variable := OCUnresolvedVariable instance.
	name := ''.
	start := 0
]

{ #category : 'testing' }
OCVariableNode >> isArgumentVariable [
	^variable isArgumentVariable
]

{ #category : 'testing' }
OCVariableNode >> isClassVariable [
	^variable isClassVariable
]

{ #category : 'testing' }
OCVariableNode >> isDefinedByBlock [
	"true if a variable node is defined by a block"
	^variable isDefinedByBlock
]

{ #category : 'testing' }
OCVariableNode >> isDefinition [
	"Check if I am a Variable defintion"
	^variable definingNode == self
]

{ #category : 'testing' }
OCVariableNode >> isGlobalVariable [
	^variable isGlobalVariable
]

{ #category : 'testing' }
OCVariableNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
OCVariableNode >> isInstanceVariable [
	^variable isInstanceVariable
]

{ #category : 'testing' }
OCVariableNode >> isLiteralVariable [
	^variable isLiteralVariable
]

{ #category : 'testing' }
OCVariableNode >> isLocalVariable [
	"returns true for temporary variables and arguments"
	^variable isLocalVariable
]

{ #category : 'testing' }
OCVariableNode >> isPseudoVariable [
	^ variable isPseudoVariable
]

{ #category : 'testing' }
OCVariableNode >> isRead [
	^ self isWrite not and: [ self isUsedAsReturnValue ]
]

{ #category : 'testing' }
OCVariableNode >> isSelfOrSuperVariable [
	^ variable isSelfOrSuperVariable
]

{ #category : 'testing' }
OCVariableNode >> isSelfVariable [
	^variable isSelfVariable
]

{ #category : 'testing' }
OCVariableNode >> isSuperVariable [
	^ variable isSuperVariable
]

{ #category : 'testing' }
OCVariableNode >> isTempVariable [
	^variable isTempVariable
]

{ #category : 'testing' }
OCVariableNode >> isThisContextVariable [
	^variable isThisContextVariable
]

{ #category : 'testing' }
OCVariableNode >> isUndeclaredVariable [

	^variable isUndeclaredVariable
]

{ #category : 'testing' }
OCVariableNode >> isVariable [
	^true
]

{ #category : 'testing' }
OCVariableNode >> isWorkspaceVariable [
	^ variable isWorkspaceVariable
]

{ #category : 'testing' }
OCVariableNode >> isWrite [
	^ self parent isNotNil and: [ self parent isAssignment and: [ self parent variable == self ] ]
]

{ #category : 'accessing' }
OCVariableNode >> name [
	^ name
]

{ #category : 'accessing' }
OCVariableNode >> name: aName [
	name := aName asSymbol
]

{ #category : 'initialization' }
OCVariableNode >> named: aName start: aPosition [

	self name: aName.
	self start: aPosition
]

{ #category : 'testing' }
OCVariableNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
OCVariableNode >> precedence [
	^0
]

{ #category : 'testing' }
OCVariableNode >> references: aVariableName [
	^self name = aVariableName
]

{ #category : 'replacing' }
OCVariableNode >> replaceSourceFrom: aNode [
	self addReplacement: (OCStringReplacement
				replaceFrom: aNode start
				to: aNode stop
				with: self name)
]

{ #category : 'replacing' }
OCVariableNode >> replaceSourceWith: aNode [
	self addReplacement: (OCStringReplacement
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : 'accessing' }
OCVariableNode >> start: aPosition [
	"Beware, start is in fact `startWithoutParentheses` as in ASTValueNode, start includes parentheses"

	start := aPosition
]

{ #category : 'accessing' }
OCVariableNode >> startForReplacement [
	"Return the start of the variable,  taking into account characters before the variable.
	This method is used when unused temporary variables are removed"
	| src t |
	src := self source.
	t := self start - 1.
	[ (src at: t) isSeparator ] whileTrue: [ t := t - 1 ].
	^ t + 1
]

{ #category : 'accessing' }
OCVariableNode >> startWithoutParentheses [
	^ start
]

{ #category : 'accessing' }
OCVariableNode >> stopWithoutParentheses [
	^ start + name size - 1
]
