example
exampleCommandLauncher
	<example>
 	^RubFloatingEditorBuilder new
		acceptOnCR: false;
		customizeEditorWith: [ :editor | 
			editor
				width: 300;
				height: 50;
				center: World center ];
		withEditedContentsDo: [ :newContents | self inform: newContents ];
		openEditorWithContents: 'self inspect'