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
	#name : 'ASTVariableNode',
	#superclass : 'ASTValueNode',
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
ASTVariableNode class >> identifierNamed: anIdentifierName at: aPosition [

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
ASTVariableNode class >> named: aName [
	^ self named: aName start: 0
]

{ #category : 'instance creation' }
ASTVariableNode class >> named: aName start: aPosition [
	^self new
		named: aName start: aPosition;
		yourself
]

{ #category : 'instance creation' }
ASTVariableNode class >> selfNode [
	^ self named: #self
]

{ #category : 'instance creation' }
ASTVariableNode class >> superNode [
	^ self named: #super
]

{ #category : 'instance creation' }
ASTVariableNode class >> thisContextNode [
	^ self named: #thisContext
]

{ #category : 'instance creation' }
ASTVariableNode class >> thisProcessNode [
	^ self named: #thisProcess
]

{ #category : 'comparing' }
ASTVariableNode >> = anObject [
	self == anObject ifTrue: [^true].
	((anObject isKindOf: self class) or: [self isKindOf: anObject class])
		ifFalse: [^false].
	^self name = anObject name
]

{ #category : 'visiting' }
ASTVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ variable acceptVisitor: aProgramNodeVisitor node: self
]

{ #category : 'matching' }
ASTVariableNode >> copyInContext: aDictionary [
	^ self class named: name
]

{ #category : 'comparing' }
ASTVariableNode >> equalTo: anObject withMapping: aDictionary [
	"If we have two variables make sure that the dictionary associates both names."
	
	^ self class = anObject class 
		and: [ (aDictionary at: self name ifAbsentPut: [ anObject name ])
		  = anObject name ]
]

{ #category : 'testing' }
ASTVariableNode >> hasIncompleteIdentifier [
	"check if there is any variable declared in my scope that starts with my name"

	"declared vars are never incomplete"
	variable isUndeclaredVariable ifFalse: [ ^false ].
	"for the others we have to search from the current scope"
	^self scope hasBindingThatBeginsWith: self name
]

{ #category : 'comparing' }
ASTVariableNode >> hash [
	^self name hash
]

{ #category : 'initialization' }
ASTVariableNode >> initialize [
	super initialize.
	variable := UnresolvedVariable instance.
	name := ''.
	start := 0
]

{ #category : 'testing' }
ASTVariableNode >> isArgumentVariable [
	^variable isArgumentVariable
]

{ #category : 'testing' }
ASTVariableNode >> isClassVariable [
	^variable isClassVariable
]

{ #category : 'testing' }
ASTVariableNode >> isDefinedByBlock [
	"true if a variable node is defined by a block"
	^variable isDefinedByBlock
]

{ #category : 'testing' }
ASTVariableNode >> isDefinition [
	"Check if I am a Variable defintion"
	^variable definingNode == self
]

{ #category : 'testing' }
ASTVariableNode >> isGlobalVariable [
	^variable isGlobalVariable
]

{ #category : 'testing' }
ASTVariableNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
ASTVariableNode >> isInstanceVariable [
	^variable isInstanceVariable
]

{ #category : 'testing' }
ASTVariableNode >> isLiteralVariable [
	^variable isLiteralVariable
]

{ #category : 'testing' }
ASTVariableNode >> isLocalVariable [
	"returns true for temporary variables and arguments"
	^variable isLocalVariable
]

{ #category : 'testing' }
ASTVariableNode >> isPseudoVariable [
	^ variable isPseudoVariable
]

{ #category : 'testing' }
ASTVariableNode >> isRead [
	^ self isWrite not and: [ self isUsedAsReturnValue ]
]

{ #category : 'testing' }
ASTVariableNode >> isSelfOrSuperVariable [
	^ variable isSelfOrSuperVariable
]

{ #category : 'testing' }
ASTVariableNode >> isSelfVariable [
	^variable isSelfVariable
]

{ #category : 'testing' }
ASTVariableNode >> isSuperVariable [
	^ variable isSuperVariable
]

{ #category : 'testing' }
ASTVariableNode >> isTempVariable [
	^variable isTempVariable
]

{ #category : 'testing' }
ASTVariableNode >> isThisContextVariable [
	^variable isThisContextVariable
]

{ #category : 'testing' }
ASTVariableNode >> isUndeclaredVariable [

	^variable isUndeclaredVariable
]

{ #category : 'testing' }
ASTVariableNode >> isVariable [
	^true
]

{ #category : 'testing' }
ASTVariableNode >> isWorkspaceVariable [
	^ variable isWorkspaceVariable
]

{ #category : 'testing' }
ASTVariableNode >> isWrite [
	^ self parent isNotNil and: [ self parent isAssignment and: [ self parent variable == self ] ]
]

{ #category : 'accessing' }
ASTVariableNode >> name [
	^ name
]

{ #category : 'accessing' }
ASTVariableNode >> name: aName [
	name := aName asSymbol
]

{ #category : 'initialization' }
ASTVariableNode >> named: aName start: aPosition [

	self name: aName.
	self start: aPosition
]

{ #category : 'testing' }
ASTVariableNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
ASTVariableNode >> precedence [
	^0
]

{ #category : 'testing' }
ASTVariableNode >> references: aVariableName [
	^self name = aVariableName
]

{ #category : 'replacing' }
ASTVariableNode >> replaceSourceFrom: aNode [
	self addReplacement: (ASTStringReplacement
				replaceFrom: aNode start
				to: aNode stop
				with: self name)
]

{ #category : 'replacing' }
ASTVariableNode >> replaceSourceWith: aNode [
	self addReplacement: (ASTStringReplacement
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : 'accessing' }
ASTVariableNode >> start: aPosition [
	"Beware, start is in fact `startWithoutParentheses` as in ASTValueNode, start includes parentheses"

	start := aPosition
]

{ #category : 'accessing' }
ASTVariableNode >> startForReplacement [
	"Return the start of the variable,  taking into account characters before the variable.
	This method is used when unused temporary variables are removed"
	| src t |
	src := self source.
	t := self start - 1.
	[ (src at: t) isSeparator ] whileTrue: [ t := t - 1 ].
	^ t + 1
]

{ #category : 'accessing' }
ASTVariableNode >> startWithoutParentheses [
	^ start
]

{ #category : 'accessing' }
ASTVariableNode >> stopWithoutParentheses [
	^ start + name size - 1
]
