Extension { #name : #IRSequence }

{ #category : #'*OpalCompiler-Tools' }
IRSequence >> gtInspectorSourceCodeIn: composite [
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
IRSequence >> gtInspectorTreeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^composite tree
		rootsExpanded;
		title: 'SourceNode';
		display: [ :anObject | {anObject sourceNode} ];
		children: [ :each | each children ];
		selectionPopulate: #selectedNode
			on: $o
			entitled: 'Open'
			with: [ :tree | tree selection ];
		format: [ :each | each printString ]
]
