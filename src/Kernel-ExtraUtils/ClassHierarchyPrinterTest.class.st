Class {
	#name : 'ClassHierarchyPrinterTest',
	#superclass : 'TestCase',
	#instVars : [
		'printer'
	],
	#category : 'Kernel-ExtraUtils-ForPharo',
	#package : 'Kernel-ExtraUtils',
	#tag : 'ForPharo'
}

{ #category : 'running' }
ClassHierarchyPrinterTest >> setUp [
	super setUp.
	printer := ClassHierarchyPrinter new
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testOnlyASTNodes [

	printer
		forClass: ASTNode;
		doNotShowState;
		doNotShowSuperclasses;
		excludedClasses: (ASTNode withAllSubclasses select: [ :each | each name beginsWith: 'RBPattern' ]);
		limitedToClasses: (ASTNode withAllSubclasses select: [:each | (each name beginsWith: 'RB') or: [ each name beginsWith: 'AST' ]]).
	self assert: printer print equals:
	'ASTNode
	ASTProgramNode
		ASTValueNode
			ASTLiteralNode
				RBLiteralArrayNode
				RBLiteralValueNode
			RBAnnotationMarkNode
			RBArrayNode
			RBAssignmentNode
			RBBlockNode
				ASTPatternBlockNode
					ASTPatternWrapperBlockNode
			RBCascadeNode
			RBMessageNode
				ASTPatternMessageNode
			RBParseErrorNode
				RBEnglobingErrorNode
					RBArrayErrorNode
					RBAssignmentErrorNode
					RBBlockErrorNode
					RBInvalidCascadeErrorNode
					RBLiteralArrayErrorNode
					RBLiteralByteArrayErrorNode
					RBParenthesesErrorNode
					RBPragmaErrorNode
					RBTemporariesErrorNode
					RBUnfinishedStatementErrorNode
			RBSelectorNode
			RBVariableNode
				ASTPatternVariableNode
		RBComment
		RBMethodNode
			ASTPatternMethodNode
			RBDoItMethodNode
		RBPragmaNode
			ASTPatternPragmaNode
		RBReturnNode
		RBSequenceNode
	RBSlotInitializationNode
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testOnlyFreeText [

	printer
		nextPutAll: 'Start';
		cr;
		tab;
		nextPutAll: 'Tabbed';
		cr;
		nextPutAll: 'End'.


	self assert: printer print equals: 'Start
	Tabbed
End'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintClassNamesOnly [

	printer
		forClass: Rectangle;
		doNotShowState;
		doNotShowSuperclasses.
	self
		assert: printer print
		equals:
'Rectangle
	CharacterBlock
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintFullHierarchyOfRectangle [

	printer forClass: Rectangle.
	self
		assert: printer print
		equals: 'ProtoObject
	Object

		Rectangle ( origin corner )
			CharacterBlock ( stringIndex text textLine )
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintHierarchyOfPoint [

	printer forClass: Point.
	self
		assert: printer print
		equals:
'ProtoObject
	Object

		Point ( x y )
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintHierarchyOfPointNoState [

	printer
		forClass: Point ;
		doNotShowState.
	self
		assert: printer print
		equals:
'ProtoObject
	Object

		Point
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintHierarchyOfPointNoSuperclasses [

	printer
		forClass: Point;
		doNotShowSuperclasses.
	self
		assert: printer print
		equals: 'Point ( x y )
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintHierarchyOfRectangle [

	printer forClass: Rectangle.
	self
		assert: printer print
		equals:
'ProtoObject
	Object

		Rectangle ( origin corner )
			CharacterBlock ( stringIndex text textLine )
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintHierarchyOfRectangleNoSuperclasses [

	printer
		forClass: Rectangle;
		doNotShowSuperclasses.
	self
		assert: printer print
		equals:
'Rectangle ( origin corner )
	CharacterBlock ( stringIndex text textLine )
'
]

{ #category : 'tests' }
ClassHierarchyPrinterTest >> testPrintHierarchyOfRectangleWithFilteredClasses [

	printer forClass: Rectangle.
	printer limitedToClasses: { Rectangle }.
	self
		assert: printer print
		equals:
'ProtoObject
	Object

		Rectangle ( origin corner )
'
]
