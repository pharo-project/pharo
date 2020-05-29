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
		'start',
		'kind'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBVariableNode class >> identifierNamed: anIdentifierName at: aPosition [
	
	anIdentifierName = 'self'
		ifTrue: [ ^ self named: anIdentifierName start: aPosition withKind: RBSelfKind ].
	anIdentifierName = 'thisContext'
		ifTrue: [ ^ self named: anIdentifierName start: aPosition withKind: RBThisContextKind ].
	anIdentifierName = 'super'
		ifTrue: [ ^ self named: anIdentifierName start: aPosition withKind: RBSuperKind ].
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

{ #category : #'instance creation' }
RBVariableNode class >> named: anIdentifierName start: aPosition withKind: aKindClass [
	
	| newOne kind |
	newOne := self named: anIdentifierName start: aPosition.
	kind := aKindClass new.
	kind variable: newOne.
	newOne kind: kind.
	
	^ newOne.
]

{ #category : #'instance creation' }
RBVariableNode class >> newArgumentNamed: aName [

	^ self named: aName start: 0 withKind: RBArgumentKind
]

{ #category : #'instance creation' }
RBVariableNode class >> newGlobalNamed: aName [

	^ self named: aName start: 0 withKind: RBGlobalKind
]

{ #category : #'instance creation' }
RBVariableNode class >> newSelfNode [

	^ self named: 'self' start: 0 withKind: RBSelfKind
]

{ #category : #'instance creation' }
RBVariableNode class >> newSuperNode [

	^ self named: 'super' start: 0 withKind: RBSuperKind
]

{ #category : #'instance creation' }
RBVariableNode class >> newTemporaryNamed: aName [

	^ self named: aName start: 0 withKind: RBTemporaryKind
]

{ #category : #'instance creation' }
RBVariableNode class >> newThisContextNode [

	^ self named: 'thisContext' start: 0 withKind: RBThisContextKind
]

{ #category : #comparing }
RBVariableNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	(anObject class = self class) 
		ifFalse: [ ^ false ].

	kind isUndefined ifTrue: [ ^ self name = anObject name ].
	anObject kind isUndefined ifTrue: [ ^ self name = anObject name ].

	^ kind class = anObject kind class 
		ifTrue: [ self name = anObject name ]
		ifFalse: [ false ].
	
]

{ #category : #visiting }
RBVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ kind acceptVisitor: aProgramNodeVisitor
]

{ #category : #converting }
RBVariableNode >> adaptToSemanticNode [
	
	kind := self binding semanticNodeKind new.
	kind variable: self.
]

{ #category : #matching }
RBVariableNode >> copyInContext: aDictionary [ 
	
	| newOne |
	newOne := self class named: name.
	newOne kind: (kind class new variable: newOne; yourself).
	^ newOne
]

{ #category : #comparing }
RBVariableNode >> equalTo: anObject withMapping: aDictionary [ 
	^self kind class = anObject kind class and: 
			[(aDictionary at: self name ifAbsentPut: [anObject name]) = anObject name]
]

{ #category : #testing }
RBVariableNode >> hasIncompleteIdentifier [
	"check if there is any variable declared in my scope that starts with my name"

	"declared vars are never incomplete"
	self binding isUndeclared ifFalse: [ ^false ].
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
	name := ''.
	start := 0.
	kind := RBUndefinedKind new.
	kind variable: self.
]

{ #category : #testing }
RBVariableNode >> isArgument [

	^ kind isArgument
]

{ #category : #testing }
RBVariableNode >> isBlockVar [

	^ kind isBlockVar
]

{ #category : #testing }
RBVariableNode >> isDefinition [
	^self parent isSequence and: [ self parent defines: name ]
]

{ #category : #testing }
RBVariableNode >> isGlobal [

	^ kind isGlobal
]

{ #category : #testing }
RBVariableNode >> isImmediateNode [
	^true
]

{ #category : #testing }
RBVariableNode >> isInstance [

	^ kind isInstance
]

{ #category : #testing }
RBVariableNode >> isRead [
	^ self isWrite not and: [ self isUsed ]
]

{ #category : #testing }
RBVariableNode >> isSelf [

	^ kind isSelf
]

{ #category : #testing }
RBVariableNode >> isSelfOrSuper [

	^ self isSelf or: [ self isSuper ]
]

{ #category : #testing }
RBVariableNode >> isSuper [
	"normally this method is not needed (if all the RBVariable creations create RBSuperNode instead but
	since we do not control this."
	
	^ self name = 'super'
]

{ #category : #testing }
RBVariableNode >> isTemp [

	^ kind isTemp
]

{ #category : #testing }
RBVariableNode >> isThisContext [

	^ kind isThisContext
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
RBVariableNode >> kind [
	^ kind
]

{ #category : #accessing }
RBVariableNode >> kind: anObject [
	kind := anObject
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

	self initialize.
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
