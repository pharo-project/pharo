Extension { #name : #RBProgramNode }

{ #category : #'*GT-InspectorExtensions-Core' }
RBProgramNode >> gtInspectorASTDumpIn: composite [
	<gtInspectorPresentationOrder: 50>
	^ composite pharoMethod
		title: [ 'AST Dump' translated ];
		display: [ (self parserClass parseExpression: self dump) formattedCode ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RBProgramNode >> gtInspectorSourceCodeIn: composite [
	<gtInspectorPresentationOrder: 30> 

	^ composite pharoMethod 
		title: [ 'Source code' translated ];
		display: [ self source ];
		smalltalkClass: [ 
			self parents first compilationContext ifNotNil: [ :ctx | ctx getClass ] ]; 
		initialize: [ :presentation | 
			presentation selectionInterval: self sourceInterval ];
		when: [ self source notNil ];
		act: [ self methodNode method compiledMethod browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'
]

{ #category : #'*GT-InspectorExtensions-Core' }
RBProgramNode >> gtInspectorTreeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTSimpleRBTreeBrowser new treeIn: composite)
		title: [ 'Tree' translated ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
RBProgramNode >> parserClass [
	^ RBParser
]
