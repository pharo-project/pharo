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
		forClass: OCNode;
		doNotShowState;
		doNotShowSuperclasses;
		excludedClasses: (OCNode withAllSubclasses select: [ :each | each name beginsWith: 'OCPattern' ]);
		limitedToClasses: (OCNode withAllSubclasses select: [:each | (each name beginsWith: 'OC')]).
	self assert: printer print equals: 'OCNode
	OCProgramNode
		OCCommentNode
		OCMethodNode
			OCDoItMethodNode
		OCPragmaNode
		OCReturnNode
		OCSequenceNode
		OCValueNode
			OCAnnotationMarkNode
			OCArrayNode
			OCAssignmentNode
			OCBlockNode
			OCCascadeNode
			OCLiteralNode
				OCLiteralArrayNode
				OCLiteralValueNode
			OCMessageNode
			OCParseErrorNode
				OCEnglobingErrorNode
					OCArrayErrorNode
					OCAssignmentErrorNode
					OCBlockErrorNode
					OCInvalidCascadeErrorNode
					OCLiteralArrayErrorNode
					OCLiteralByteArrayErrorNode
					OCParenthesesErrorNode
					OCPragmaErrorNode
					OCTemporariesErrorNode
					OCUnfinishedStatementErrorNode
			OCSelectorNode
			OCVariableNode
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
