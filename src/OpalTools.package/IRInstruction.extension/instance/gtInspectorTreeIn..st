*OpalTools
gtInspectorTreeIn: composite
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
		format: [ :each | 
					String
						streamContents: [ :stream | 
							stream
								nextPutAll: each class name;
								nextPut: $(;
								nextPutAll: ((each formattedCode truncateTo: 50) copyReplaceAll: String cr with: String space);
								nextPut: $) ] ]