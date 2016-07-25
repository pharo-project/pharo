examples
exampleEditableList1
	<example>
	| list data|
	data := OrderedCollection with: 'A' with: 'B' with: 'C'.
	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleDropDataSource new elements: data);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | |index morph ed |
				index := ann selectedRowIndex.
				morph := list visibleRowMorphAtIndex: index.
				ed := RubFloatingEditorBuilder new
					customizeEditorWith: [ :editor | 
						editor scrollbarsShowNever.
						editor bounds: (morph bounds insetBy: (Margin left: -2 top: 0 right: 0 bottom: 0  ))
					];
				withEditedContentsDo: [ :editedContents | 
					data at: index put: editedContents asString.
					list refresh].
				ed openEditorWithContents: (data at: index) 
		];
		yourself.
		
	^ list openInWindow