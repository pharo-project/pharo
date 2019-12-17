"
A visitor that does the typing for better completion. No type guessing, just using the information that's already known to the system
"
Class {
	#name : #TypingVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'typeStack'
	],
	#category : #'NECompletion-Model'
}

{ #category : #visiting }
TypingVisitor >> initialize [

	typeStack := Stack new.
	typeStack push: Dictionary new.
]

{ #category : #visiting }
TypingVisitor >> visitArrayNode: aLiteralNode [
	 aLiteralNode propertyAt: #type put: Array
]

{ #category : #visiting }
TypingVisitor >> visitAssignmentNode: anAssignmentNode [
	"if not a temporary variable - return.
	if no type return.
	add type of a variable to a dictionary with the key being the variable's name"
	
	| variableType |
	super visitAssignmentNode: anAssignmentNode.
	anAssignmentNode variable isTemp ifFalse: [ ^self ].
   (anAssignmentNode value hasProperty: #type) ifFalse: [ ^self ].
	variableType := anAssignmentNode value propertyAt: #type.
	typeStack top at: anAssignmentNode variable name put: variableType.
]

{ #category : #visiting }
TypingVisitor >> visitBlockNode: aBlockNode [
	"inside a stack we push a copy of an already existing dictionary (with the state of variables before the block) because in the block we want to know the previous state of variables, and then if inside the block the type of the variable changes we update our copy. after the block exit if the variable type has changed explicitly again, then we take after-block change, otherwise we take the last before-block type and the last inside-block type and if they are the same the type stays the same, otherwise it's updated to their common superclass, because we don't know if the block executes so want suggestions from either"
	| stackTop |
	typeStack push: typeStack top copy.
	super visitBlockNode: aBlockNode.
	stackTop := typeStack pop.
	stackTop keysDo: [ :name | 
			| type1 type2 |
			type2 := stackTop at: name.
			(typeStack top includesKey: name)
				ifFalse: [ typeStack top at: name put: type2 ]
				ifTrue: [ type1 := typeStack top at: name. 
					type1 == type2 ifFalse: [ typeStack top at: name put: (type2 commonSuperclassWith: type1) ] ] ].
	aBlockNode propertyAt: #type put: BlockClosure
]

{ #category : #visiting }
TypingVisitor >> visitGlobalNode: aGlobalNode [
	aGlobalNode
		propertyAt: #type
		put: aGlobalNode binding read class
]

{ #category : #visiting }
TypingVisitor >> visitLiteralArrayNode: aLiteralNode [
	 aLiteralNode propertyAt: #type put: Array
]

{ #category : #visiting }
TypingVisitor >> visitLiteralNode: aLiteralNode [
	 aLiteralNode propertyAt: #type put: aLiteralNode value class
]

{ #category : #visiting }
TypingVisitor >> visitSelfNode: aSelfNode [
	aSelfNode propertyAt: #type put: aSelfNode methodNode methodClass
]

{ #category : #visiting }
TypingVisitor >> visitSuperNode: aSuperNode [
	| class | 
	class := aSuperNode methodNode methodClass.
	"the type of super of a Trait is not known"
	class isTrait ifTrue: [ ^self ].
	aSuperNode propertyAt: #type put: class superclass
]

{ #category : #visiting }
TypingVisitor >> visitTemporaryNode: aTemporaryNode [
	(typeStack top includesKey: aTemporaryNode name) ifFalse: [^self ].
	aTemporaryNode propertyAt: #type put: (typeStack top at: aTemporaryNode name)
]
