Extension { #name : #IRInstruction }

{ #category : #'*OpalCompiler-Tools' }
IRInstruction >> gtInspectorSourceCodeIn: composite [
	<gtInspectorPresentationOrder: 30> 

	^ composite pharoMethod 
		title: 'Source code';
		display: [ self method sourceNode source];
		smalltalkClass: [  self method sourceNode methodClass ]; 
		initialize: [ :presentation | 
			presentation selectionInterval: self sourceInterval ];
		act: [ self method compiledMethod browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'
]

{ #category : #'*OpalCompiler-Tools' }
IRInstruction >> gtInspectorTreeIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite tree
		rootsExpanded;
		title: 'SourceNode';
		display: [ :anObject | {anObject sourceNode} ];
		children: [ :each | each children ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | 
			String
				streamContents: [ :stream | 
					stream
						nextPutAll: each class name;
						nextPut: $(;
						nextPutAll: ((each formattedCode truncateTo: 50) copyReplaceAll: String newLine with: String space);
						nextPut: $) ] ]
]
