"
I cache a mapping between possible bytecode offsets and the AST nodes they correspond to for a given compiled method.

Instanciate me using my class method generateForCompiledMethod: and give me as parameter a compiled method.

Use me through the node access API method nodeForPC: and give me a program counter as parameter.

I store: 
- firstBcOffset: The first bytecode pc. If you try to access a pc below that first pc, I return the method node.
- lastBcOffset: The last bytecode pc. If you try to access a pc after this last pc, I return the node associated with the last pc.
- bcToASTMap: A map associating each possible pc between firstBcOffset and lastBcOffset and the corresponding ast node.
- the methode node.
"
Class {
	#name : #SindarinBytecodeToASTCache,
	#superclass : #Object,
	#instVars : [
		'firstBcOffset',
		'lastBcOffset',
		'bcToASTMap',
		'methodNode'
	],
	#category : #Sindarin
}

{ #category : #initialization }
SindarinBytecodeToASTCache class >> generateForCompiledMethod: compiledMethod [
	^self new generateForCompiledMethod: compiledMethod
]

{ #category : #accessing }
SindarinBytecodeToASTCache >> bcToASTMap [
	^ bcToASTMap
]

{ #category : #private }
SindarinBytecodeToASTCache >> fillMissingBCOffsetsWithLastBCOffsetNodes [
	"It happens that different bytecode offsets map to the same AST node.
	These cases are detected when there is no node mapped between, for example, bcOffset 46 and bcOffset 50.
	In that case, we take every possible bytecode index between 46 and 50 (i.e., 47, 48, 49),
	and we map them to the same node as the last mapped bytecode offset, here 46."

	| sortedBCOffsets |
	sortedBCOffsets := bcToASTMap keys asSortedCollection.
	1 to: sortedBCOffsets size - 1 do: [ :index | 
		| bcAtIndex bcAtNextIndex |
		bcAtIndex := sortedBCOffsets at: index.
		bcAtNextIndex := sortedBCOffsets at: index + 1.
		bcAtIndex < bcAtNextIndex ifTrue: [ 
			bcAtIndex to: bcAtNextIndex - 1 do: [ :i | 
			bcToASTMap at: i put: (bcToASTMap at: bcAtIndex) ] ] ]
]

{ #category : #accessing }
SindarinBytecodeToASTCache >> firstBcOffset [
	^ firstBcOffset
]

{ #category : #initialization }
SindarinBytecodeToASTCache >> generateForCompiledMethod: compiledMethod [
	| methodIR currentBcOffset |
	methodNode := compiledMethod ast.
	methodIR := methodNode ir.
	bcToASTMap := Dictionary new.
	firstBcOffset := compiledMethod initialPC.
	currentBcOffset := firstBcOffset.
	methodIR startSequence withAllSuccessors do: [ :seq | 
		seq do: [ :ir | 
			ir ifNotNil: [ 
				bcToASTMap at: ir bytecodeOffset ifAbsentPut: [ ir sourceNode ].
				currentBcOffset := ir bytecodeOffset + 1 ] ] ].
	lastBcOffset := currentBcOffset - 1.
	self fillMissingBCOffsetsWithLastBCOffsetNodes
]

{ #category : #accessing }
SindarinBytecodeToASTCache >> lastBcOffset [
	^ lastBcOffset
]

{ #category : #accessing }
SindarinBytecodeToASTCache >> methodNode [
	^ methodNode
]

{ #category : #'node access' }
SindarinBytecodeToASTCache >> nodeForPC: pc [
	pc < firstBcOffset ifTrue: [ ^ methodNode ].
	pc > lastBcOffset ifTrue: [ ^ bcToASTMap at: lastBcOffset ].
	^ bcToASTMap at: pc
]
