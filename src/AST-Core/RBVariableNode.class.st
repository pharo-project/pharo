"
RBVariableNode is an AST node that represents a variable (global, inst var, temp, etc.).

Although this is the basic class for the concrete variable types, this is not an abstract class and is actually used
by the parser for all variables that aren't special builtin types like self/super/thisContext. All other variables are
just RBVariableNodes until the semantic analyser can deduce the type.

Instance Variables:
	name	<RBValueToken>	the variable's name I represent
	nameStart <Integer>	the position where I was found at the source code

"
Class {
	#name : #RBVariableNode,
	#superclass : #RBValueNode,
	#instVars : [
		'name',
		'variable',
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBVariableNode class >> identifierNamed: anIdentifierName at: aPosition [

	anIdentifierName = #self
		ifTrue: [ ^ self selfNode start: aPosition ].
	anIdentifierName = #thisContext
		ifTrue: [ ^ self thisContextNode start: aPosition ].
	anIdentifierName = #super
		ifTrue: [ ^ self superNode start: aPosition ].
	^ self named: anIdentifierName start: aPosition
]

{ #category : #'instance creation' }
RBVariableNode class >> named: aName [
	^ self named: aName start: 0
]

{ #category : #'instance creation' }
RBVariableNode class >> named: aName start: aPosition [
	^self new
		named: aName start: aPosition;
		yourself
]

{ #category : #'instance creation' }
RBVariableNode class >> selfNode [
	^ (self named: #self) variable: SelfVariable instance
]

{ #category : #'instance creation' }
RBVariableNode class >> superNode [
	^ (self named: #super) variable: SuperVariable instance
]

{ #category : #'instance creation' }
RBVariableNode class >> thisContextNode [
	^ (self named: #thisContext) variable: ThisContextVariable instance
]

{ #category : #comparing }
RBVariableNode >> = anObject [
	self == anObject ifTrue: [^true].
	((anObject isKindOf: self class) or: [self isKindOf: anObject class])
		ifFalse: [^false].
	^self name = anObject name
]

{ #category : #visiting }
RBVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ variable acceptVisitor: aProgramNodeVisitor node: self
]

{ #category : #matching }
RBVariableNode >> copyInContext: aDictionary [
	^ self class named: name
]

{ #category : #comparing }
RBVariableNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class and:
			[(aDictionary at: self name ifAbsentPut: [anObject name]) = anObject name]
]

{ #category : #testing }
RBVariableNode >> hasIncompleteIdentifier [
	"check if there is any variable declared in my scope that starts with my name"

	"declared vars are never incomplete"
	variable isUndeclaredVariable ifFalse: [ ^false ].
	"for the others we have to search from the current scope"
	^self scope hasBindingThatBeginsWith: self name
]

{ #category : #comparing }
RBVariableNode >> hash [
	^self name hash
]

{ #category : #initialization }
RBVariableNode >> initialize [
	super initialize.
	variable := UnresolvedVariable instance.
	name := ''.
	start := 0
]

{ #category : #testing }
RBVariableNode >> isArgumentVariable [
	^variable isArgumentVariable
]

{ #category : #testing }
RBVariableNode >> isClassVariable [
	^variable isClassVariable
]

{ #category : #testing }
RBVariableNode >> isDefinedByBlock [
	"true if a variable node is defined by a block"
	^variable isDefinedByBlock
]

{ #category : #testing }
RBVariableNode >> isDefinition [
	"Check if I am a Variable defintion"
	^variable definingNode == self
]

{ #category : #testing }
RBVariableNode >> isGlobalVariable [
	^variable isGlobalVariable
]

{ #category : #testing }
RBVariableNode >> isImmediateNode [
	^true
]

{ #category : #testing }
RBVariableNode >> isInstanceVariable [
	^variable isInstanceVariable
]

{ #category : #testing }
RBVariableNode >> isLiteralVariable [
	^variable isLiteralVariable
]

{ #category : #testing }
RBVariableNode >> isLocalVariable [
	"returns true for temporary variables and arguments"
	^variable isLocalVariable
]

{ #category : #testing }
RBVariableNode >> isRead [
	^ self isWrite not and: [ self isUsed ]
]

{ #category : #testing }
RBVariableNode >> isReservedVariable [
	^ variable isReservedVariable
]

{ #category : #testing }
RBVariableNode >> isSelfOrSuperVariable [
	^ variable isSelfOrSuperVariable
]

{ #category : #testing }
RBVariableNode >> isSelfVariable [
	^variable isSelfVariable
]

{ #category : #testing }
RBVariableNode >> isSuperVariable [
	^ variable isSuperVariable
]

{ #category : #testing }
RBVariableNode >> isTempVariable [
	^variable isTempVariable
]

{ #category : #testing }
RBVariableNode >> isThisContextVariable [
	^variable isThisContextVariable
]

{ #category : #testing }
RBVariableNode >> isUndeclaredVariable [

	^variable isUndeclaredVariable
]

{ #category : #testing }
RBVariableNode >> isVariable [
	^true
]

{ #category : #testing }
RBVariableNode >> isWorkspaceVariable [
	^ variable isWorkspaceVariable
]

{ #category : #testing }
RBVariableNode >> isWrite [
	^ self parent notNil and: [ self parent isAssignment and: [ self parent variable == self ] ]
]

{ #category : #accessing }
RBVariableNode >> name [
	^ name
]

{ #category : #accessing }
RBVariableNode >> name: aName [
	name := aName asSymbol
]

{ #category : #initialization }
RBVariableNode >> named: aName start: aPosition [

	self name: aName.
	self start: aPosition
]

{ #category : #testing }
RBVariableNode >> needsParenthesis [
	^false
]

{ #category : #accessing }
RBVariableNode >> precedence [
	^0
]

{ #category : #testing }
RBVariableNode >> references: aVariableName [
	^self name = aVariableName
]

{ #category : #replacing }
RBVariableNode >> replaceSourceFrom: aNode [
	self addReplacement: (RBStringReplacement
				replaceFrom: aNode start
				to: aNode stop
				with: self name)
]

{ #category : #replacing }
RBVariableNode >> replaceSourceWith: aNode [
	self addReplacement: (RBStringReplacement
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : #accessing }
RBVariableNode >> start [
	^ start
]

{ #category : #accessing }
RBVariableNode >> start: aPosition [
	start := aPosition
]

{ #category : #accessing }
RBVariableNode >> startForReplacement [
	"Return the start of the variable,  taking into account characters before the variable.
	This method is used when unused temporary variables are removed"
	| src t |
	src := self source.
	t := self start - 1.
	[ (src at: t) isSeparator ] whileTrue: [ t := t - 1 ].
	^ t + 1
]

{ #category : #accessing }
RBVariableNode >> startWithoutParentheses [
	^ start
]

{ #category : #accessing }
RBVariableNode >> stopWithoutParentheses [
	^ start + name size - 1
]
