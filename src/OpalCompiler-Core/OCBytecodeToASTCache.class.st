"
I cache a mapping between possible bytecode offsets and the AST nodes that correspond to for a given compiled method.

- Instanciate me using my class method `generateForCompiledMethod:` and give me as parameter a compiled method.
- Use me through the node access API method `nodeForPC:` and give me a program counter as parameter.

I store: 
- `firstBcOffset:` The first bytecode pc. If you try to access a pc below that first pc, I return the method node.
- `lastBcOffset:` The last bytecode pc. If you try to access a pc after this last pc, I return the node associated with the last pc.
- `bcToASTMap:` A map associating each possible pc between firstBcOffset and lastBcOffset and the corresponding ast node.
- the methode node.
"
Class {
	#name : 'OCBytecodeToASTCache',
	#superclass : 'Object',
	#instVars : [
		'firstBcOffset',
		'lastBcOffset',
		'bcToASTMap',
		'methodOrBlockNode'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'initialization' }
OCBytecodeToASTCache class >> generateForNode: aNode [
	^self new generateForNode: aNode
]

{ #category : 'accessing' }
OCBytecodeToASTCache >> bcToASTMap [
	^ bcToASTMap
]

{ #category : 'private' }
OCBytecodeToASTCache >> fillMissingBCOffsetsWithLastBCOffsetNodes [
	 "It happens that different bytecode offsets map to the same AST node.
	These cases are detected when there is no node mapped between, for example, bcOffset 46 and bcOffset 50.
	In that case, we take every possible bytecode index between 46 and 50 (i.e., 47, 48, 49),
	and we map them to the same node as the last mapped bytecode offset, here 46.

	When a fullblocks is the first node in a method body, it is possible (when?) the offsets do not start at the first offset of the method (why?).
	In that case, we also fix the missing offsets by mapping to them the first mapped node."

	 | sortedBCOffsets |
	 sortedBCOffsets := bcToASTMap keys asSortedCollection.
	 sortedBCOffsets first = firstBcOffset ifFalse: [
		 firstBcOffset to: sortedBCOffsets first - 1 do: [ :offset |
			 bcToASTMap at: offset put: (bcToASTMap at: sortedBCOffsets first) ] ].
	 1 to: sortedBCOffsets size - 1 do: [ :index |
		 | bcAtIndex bcAtNextIndex |
		 bcAtIndex := sortedBCOffsets at: index.
		 bcAtNextIndex := sortedBCOffsets at: index + 1.
		 bcAtIndex < bcAtNextIndex ifTrue: [
			 bcAtIndex to: bcAtNextIndex - 1 do: [ :i |
			 bcToASTMap at: i put: (bcToASTMap at: bcAtIndex) ] ] ]
]

{ #category : 'accessing' }
OCBytecodeToASTCache >> firstBcOffset [
	^ firstBcOffset
]

{ #category : 'astAndAstMapping' }
OCBytecodeToASTCache >> firstBcOffsetForNode: aNode [

	^ (self pcsForNode: aNode) at: 1 ifAbsent: nil
]

{ #category : 'initialization' }
OCBytecodeToASTCache >> generateForNode: aNode [
	| methodOrBlockIr currentBcOffset |
	methodOrBlockNode := aNode.
	methodOrBlockIr := aNode ir.
	bcToASTMap := Dictionary new.
	firstBcOffset := methodOrBlockIr compiledMethod initialPC.

	"if there is a primitive, we need to record the offset"
	(methodOrBlockIr irPrimitive num ~= 0
		or: [ methodOrBlockIr compiledMethod isQuick ])
		ifTrue: [ bcToASTMap at: firstBcOffset put: methodOrBlockNode ].

	currentBcOffset := firstBcOffset.
	methodOrBlockIr startSequence withAllSuccessors
		do: [ :seq |
			seq
				do: [ :ir |
					ir
						ifNotNil: [ bcToASTMap at: ir bytecodeOffset ifAbsentPut: [ ir sourceNode ].
							currentBcOffset := ir bytecodeOffset + 1 ] ] ].
	lastBcOffset := currentBcOffset - 1.
	self fillMissingBCOffsetsWithLastBCOffsetNodes
]

{ #category : 'accessing' }
OCBytecodeToASTCache >> lastBcOffset [
	^ lastBcOffset
]

{ #category : 'astAndAstMapping' }
OCBytecodeToASTCache >> lastBcOffsetForNode: aNode [

	| pcsForNode |
	pcsForNode := self pcsForNode: aNode.
	^ pcsForNode at: pcsForNode size ifAbsent: nil
]

{ #category : 'accessing' }
OCBytecodeToASTCache >> methodNode [
	^ methodOrBlockNode
]

{ #category : 'accessing' }
OCBytecodeToASTCache >> methodOrBlockNode [
	^ methodOrBlockNode
]

{ #category : 'node access' }
OCBytecodeToASTCache >> nodeForPC: pc [
	pc < firstBcOffset ifTrue: [ ^ methodOrBlockNode ].
	pc > lastBcOffset ifTrue: [ ^ bcToASTMap at: lastBcOffset ].
	^ bcToASTMap at: pc
]

{ #category : 'astAndAstMapping' }
OCBytecodeToASTCache >> pcsForNode: aNode [

	^ (bcToASTMap keys select: [ :key | (bcToASTMap at: key) == aNode ]) asOrderedCollection sorted
]
