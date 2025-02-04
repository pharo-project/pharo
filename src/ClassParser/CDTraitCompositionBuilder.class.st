"
I am in charge of building a Trait composition. 

aTraitCompositionNode is composed of TraitNode.
"
Class {
	#name : 'CDTraitCompositionBuilder',
	#superclass : 'Object',
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'building' }
CDTraitCompositionBuilder >> buildFrom: aNode [

	^ aNode acceptVisitor: self
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitArrayNode: anArrayNode [
	| parent argumentName |
	self flag: #todo.
	"It needs to be refactored. I do not know how it was supposed to be handle properly.
	Here is workaround to fix issue with empty #uses section"
	parent := anArrayNode parent.
	parent isMessage ifTrue: [
		argumentName := parent keywords at: (parent arguments indexOf: anArrayNode).
		(argumentName = #uses:) | (argumentName = #traits:) ifTrue: [
			anArrayNode statements
				ifEmpty: [
					^CDTraitCompositionSequenceNode new
						sequence: #();
						originalNode: anArrayNode;
						yourself]
				ifNotEmpty: [ :stm | ^ CDTraitCompositionSequenceNode new
											sequence: {stm first acceptVisitor: self} ;
											originalNode: anArrayNode;
											yourself
									]]].

	^anArrayNode statements collect: [ :each |
		each isLiteralNode
			ifTrue: [ each value ]
			ifFalse: [ each receiver value -> each arguments first value ] ]
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitGlobalNode: aGlobalNode [

	^self visitVariableNode: aGlobalNode
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitGlobalVariableNode: aGlobalNode [

	^self visitVariableNode: aGlobalNode
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitLiteralNode: aLiteralValueNode [

	^ aLiteralValueNode value
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitLiteralValueNode: aLiteralValueNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aLiteralValueNode
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitLiteralVariableNode: aGlobalNode [

	^self visitVariableNode: aGlobalNode
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitMessageNode: aMessageNode [

	| node receiver argument |

	aMessageNode selector = #classTrait
		ifTrue: [
			^ CDClassTraitNode new
				originalNode: aMessageNode;
				name: aMessageNode receiver name;
				yourself  ].

	receiver := aMessageNode receiver acceptVisitor: self.
	argument := aMessageNode arguments first acceptVisitor: self.

	node := receiver perform: aMessageNode selector with: argument.
	node originalNode: aMessageNode.
	^ node
]

{ #category : 'visiting' }
CDTraitCompositionBuilder >> visitVariableNode: aVariableNode [

	^ CDTraitNode new
		originalNode: aVariableNode;
		name: aVariableNode name;
		yourself
]
