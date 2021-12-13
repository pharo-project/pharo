"
For the Class part:  I represent a visitor for the RB AST which allows to dynamically set the behavior for each kind of nodes.

You can register a block for any kind of node using the message #registerBlock:for:
The kind of node is taken from the visit method name: visitMethodNode => #methodNode
I works as any other visitor !
 
Example:
Counting the number of argument nodes
| visitor count |
count := 0.
visitor := RBAnonymousVisitor new
	registerBlock: [ count := count + 1 ] for: #argumentNodes;
	yourself.
(self >> #visitArgumentNode:) ast acceptVisitor: visitor.
count >>> 2
"
Class {
	#name : #RBAnonymousVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'visits'
	],
	#category : #'AST-Core-Visitors'
}

{ #category : #initialization }
RBAnonymousVisitor >> initialize [
	visits := Dictionary new
]

{ #category : #configuring }
RBAnonymousVisitor >> registerBlock: aBlock for: aKindOfNode [

	(#( 'englobingErrorNode' 'selectorNode' 'storePopIntoTempNode'
	    'temporaryVariableNode' 'literalVariableNode'
	    'classVariableNode' 'patternWrapperBlockNode'
	    'literalNode' 'blockNode' 'sequenceNode' 'returnNode'
	    'argumentNodes' 'patternBlockNode' 'slotInitializationNode'
	    'temporaryNode' 'selfNode' 'argumentNode' 'node' 'parseErrorNode'
	    'localVariableNode' 'storeIntoTempNode' 'arrayNode' 'assignmentNode'
	    'variableNode' 'globalVariableNode' 'instanceVariableNode'
	    'temporaryNodes' 'cascadeNode' 'argumentVariableNode'
	    'temporaryDeclarationNode' 'messageNode' 'pragmaNode'
	    'globalNode' 'methodNode' 'unreachableStatement'
	    'literalArrayNode' 'thisContextNode' 'superNode' 'literalValueNode' ) 
		 includes: aKindOfNode) ifFalse: [ 
		self error: aKindOfNode , ' does not seem to exist.' ].
	visits at: #aKindOfNode put: aBlock
]

{ #category : #visiting }
RBAnonymousVisitor >> visitArgumentNode: argumentNode [

	visits
		at: #argumentNode
		ifPresent: [ :aBlock | aBlock cull: argumentNode ].
	^ super visitArgumentNode: argumentNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitArgumentNodes: argumentNodes [

	visits
		at: #argumentNodes
		ifPresent: [ :aBlock | aBlock cull: argumentNodes ].
	^ super visitArgumentNodes: argumentNodes
]

{ #category : #'visiting - variables' }
RBAnonymousVisitor >> visitArgumentVariableNode: argumentVariableNode [

	visits
		at: #argumentVariableNode
		ifPresent: [ :aBlock | aBlock cull: argumentVariableNode ].
	^ super visitArgumentVariableNode: argumentVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitArrayNode: arrayNode [

	visits at: #arrayNode ifPresent: [ :aBlock | aBlock cull: arrayNode ].
	^ super visitArrayNode: arrayNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitAssignmentNode: assignmentNode [

	visits
		at: #assignmentNode
		ifPresent: [ :aBlock | aBlock cull: assignmentNode ].
	^ super visitAssignmentNode: assignmentNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitBlockNode: blockNode [

	visits at: #blockNode ifPresent: [ :aBlock | aBlock cull: blockNode ].
	^ super visitBlockNode: blockNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitCascadeNode: cascadeNode [

	visits
		at: #cascadeNode
		ifPresent: [ :aBlock | aBlock cull: cascadeNode ].
	^ super visitCascadeNode: cascadeNode
]

{ #category : #'visiting - variables' }
RBAnonymousVisitor >> visitClassVariableNode: classVariableNode [

	visits
		at: #classVariableNode
		ifPresent: [ :aBlock | aBlock cull: classVariableNode ].
	^ super visitClassVariableNode: classVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitEnglobingErrorNode: englobingErrorNode [

	visits
		at: #englobingErrorNode
		ifPresent: [ :aBlock | aBlock cull: englobingErrorNode ].
	^ super visitEnglobingErrorNode: englobingErrorNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitGlobalNode: globalNode [

	visits
		at: #globalNode
		ifPresent: [ :aBlock | aBlock cull: globalNode ].
	^ super visitGlobalNode: globalNode
]

{ #category : #'visiting - variables' }
RBAnonymousVisitor >> visitGlobalVariableNode: globalVariableNode [

	visits
		at: #globalVariableNode
		ifPresent: [ :aBlock | aBlock cull: globalVariableNode ].
	^ super visitGlobalVariableNode: globalVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitInstanceVariableNode: instanceVariableNode [

	visits
		at: #instanceVariableNode
		ifPresent: [ :aBlock | aBlock cull: instanceVariableNode ].
	^ super visitInstanceVariableNode: instanceVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitLiteralArrayNode: literalArrayNode [

	visits
		at: #literalArrayNode
		ifPresent: [ :aBlock | aBlock cull: literalArrayNode ].
	^ super visitLiteralArrayNode: literalArrayNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitLiteralNode: literalNode [

	visits
		at: #literalNode
		ifPresent: [ :aBlock | aBlock cull: literalNode ].
	^ super visitLiteralNode: literalNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitLiteralValueNode: literalValueNode [

	visits
		at: #literalValueNode
		ifPresent: [ :aBlock | aBlock cull: literalValueNode ].
	^ super visitLiteralValueNode: literalValueNode
]

{ #category : #'visiting - variables' }
RBAnonymousVisitor >> visitLiteralVariableNode: literalVariableNode [

	visits
		at: #literalVariableNode
		ifPresent: [ :aBlock | aBlock cull: literalVariableNode ].
	^ super visitLiteralVariableNode: literalVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitLocalVariableNode: localVariableNode [

	visits
		at: #localVariableNode
		ifPresent: [ :aBlock | aBlock cull: localVariableNode ].
	^ super visitLocalVariableNode: localVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitMessageNode: messageNode [

	visits
		at: #messageNode
		ifPresent: [ :aBlock | aBlock cull: messageNode ].
	^ super visitMessageNode: messageNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitMethodNode: methodNode [

	visits
		at: #methodNode
		ifPresent: [ :aBlock | aBlock cull: methodNode ].
	^ super visitMethodNode: methodNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitNode: node [

	visits at: #node ifPresent: [ :aBlock | aBlock cull: node ].
	^ super visitNode: node
]

{ #category : #visiting }
RBAnonymousVisitor >> visitParseErrorNode: parseErrorNode [

	visits
		at: #parseErrorNode
		ifPresent: [ :aBlock | aBlock cull: parseErrorNode ].
	^ super visitParseErrorNode: parseErrorNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitPatternBlockNode: patternBlockNode [

	visits
		at: #patternBlockNode
		ifPresent: [ :aBlock | aBlock cull: patternBlockNode ].
	^ super visitPatternBlockNode: patternBlockNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitPatternWrapperBlockNode: patternWrapperBlockNode [

	visits
		at: #patternWrapperBlockNode
		ifPresent: [ :aBlock | aBlock cull: patternWrapperBlockNode ].
	^ super visitPatternWrapperBlockNode: patternWrapperBlockNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitPragmaNode: pragmaNode [

	visits
		at: #pragmaNode
		ifPresent: [ :aBlock | aBlock cull: pragmaNode ].
	^ super visitPragmaNode: pragmaNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitReturnNode: returnNode [

	visits
		at: #returnNode
		ifPresent: [ :aBlock | aBlock cull: returnNode ].
	^ super visitReturnNode: returnNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitSelectorNode: selectorNode [

	visits
		at: #selectorNode
		ifPresent: [ :aBlock | aBlock cull: selectorNode ].
	^ super visitSelectorNode: selectorNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitSelfNode: selfNode [

	visits at: #selfNode ifPresent: [ :aBlock | aBlock cull: selfNode ].
	^ super visitSelfNode: selfNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitSequenceNode: sequenceNode [

	visits
		at: #sequenceNode
		ifPresent: [ :aBlock | aBlock cull: sequenceNode ].
	^ super visitSequenceNode: sequenceNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitSuperNode: superNode [

	visits at: #superNode ifPresent: [ :aBlock | aBlock cull: superNode ].
	^ super visitSuperNode: superNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitTemporaryDeclarationNode: temporaryDeclarationNode [

	visits
		at: #temporaryDeclarationNode
		ifPresent: [ :aBlock | aBlock cull: temporaryDeclarationNode ].
	^ super visitTemporaryDeclarationNode: temporaryDeclarationNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitTemporaryNode: temporaryNode [

	visits
		at: #temporaryNode
		ifPresent: [ :aBlock | aBlock cull: temporaryNode ].
	^ super visitTemporaryNode: temporaryNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitTemporaryNodes: temporaryNodes [

	visits
		at: #temporaryNodes
		ifPresent: [ :aBlock | aBlock cull: temporaryNodes ].
	^ super visitTemporaryNodes: temporaryNodes
]

{ #category : #'visiting - variables' }
RBAnonymousVisitor >> visitTemporaryVariableNode: temporaryVariableNode [

	visits
		at: #temporaryVariableNode
		ifPresent: [ :aBlock | aBlock cull: temporaryVariableNode ].
	^ super visitTemporaryVariableNode: temporaryVariableNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitThisContextNode: thisContextNode [

	visits
		at: #thisContextNode
		ifPresent: [ :aBlock | aBlock cull: thisContextNode ].
	^ super visitThisContextNode: thisContextNode
]

{ #category : #visiting }
RBAnonymousVisitor >> visitUnreachableStatement: unreachableStatement [

	visits
		at: #unreachableStatement
		ifPresent: [ :aBlock | aBlock cull: unreachableStatement ].
	^ super visitUnreachableStatement: unreachableStatement
]

{ #category : #visiting }
RBAnonymousVisitor >> visitVariableNode: variableNode [

	visits
		at: #variableNode
		ifPresent: [ :aBlock | aBlock cull: variableNode ].
	^ super visitVariableNode: variableNode
]
