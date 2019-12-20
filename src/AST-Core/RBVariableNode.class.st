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
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBVariableNode class >> identifierNamed: anIdentifierName at: aPosition [
	
	anIdentifierName = 'self'
		ifTrue: [ ^ RBSelfNode named: anIdentifierName start: aPosition ].
	anIdentifierName = 'thisContext'
		ifTrue: [ ^ RBThisContextNode named: anIdentifierName start: aPosition ].
	anIdentifierName = 'super'
		ifTrue: [ ^ RBSuperNode named: anIdentifierName start: aPosition ].
	^ self named: anIdentifierName start: aPosition.
]

{ #category : #'instance creation' }
RBVariableNode class >> named: aName [
	^self named: aName start: 0.
]

{ #category : #'instance creation' }
RBVariableNode class >> named: aName start: aPosition [
	^self new 
		named: aName start: aPosition; 
		yourself.
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
	^ aProgramNodeVisitor visitVariableNode: self
]

{ #category : #converting }
RBVariableNode >> adaptToSemanticNode [
	
	self primitiveChangeClassTo: self binding semanticNodeClass new
]

{ #category : #matching }
RBVariableNode >> copyInContext: aDictionary [ 
	^ self class named: name.
]

{ #category : #comparing }
RBVariableNode >> equalTo: anObject withMapping: aDictionary [ 
	^self class = anObject class and: 
			[(aDictionary at: self name ifAbsentPut: [anObject name]) = anObject name]
]

{ #category : #testing }
RBVariableNode >> hasIncompleteIdentifier [
	^ self binding isUndeclared
		ifTrue: [ (self scope findVariable: [ :v | v name beginsWith: self name ] ifNone: [ nil ]) notNil]
]

{ #category : #comparing }
RBVariableNode >> hash [
	^self name hash
]

{ #category : #initialization }
RBVariableNode >> initialize [
	super initialize.
	name := ''.
	start := 0
]

{ #category : #testing }
RBVariableNode >> isDefinition [
	^self parent isSequence and: [ self parent defines: name ]
]

{ #category : #testing }
RBVariableNode >> isImmediateNode [
	^true
]

{ #category : #testing }
RBVariableNode >> isRead [
	^ self isWrite not and: [ self isUsed ]
]

{ #category : #testing }
RBVariableNode >> isSelf [
	"normally this method is not needed (if all the RBVariable creations create RBSelfNode instead but
	since we do not control this."
	
	^ self name = 'self'
]

{ #category : #testing }
RBVariableNode >> isSuper [
	"normally this method is not needed (if all the RBVariable creations create RBSuperNode instead but
	since we do not control this."
	
	^ self name = 'super'
]

{ #category : #testing }
RBVariableNode >> isVariable [
	^true
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
	name := aName asSymbol.
]

{ #category : #initialization }
RBVariableNode >> named: aName start: aPosition [

	self name: aName.
	self start: aPosition.
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
	^ start + name size - 1.
]
