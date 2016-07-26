services
chooseFullFileNameIn: aThemedMorph title: title extensions: exts path: path preview: preview
	"Answer the result of a file name chooser dialog with the given title, extensions
	path and preview type.
	Answer nil or a full filename."

	|fd|
	fd := FileDialogWindow basicNew
		basicTheme: aThemedMorph theme;
		previewType: preview;
		initialize;
		title: title;
		answerPathName.
	exts ifNotNil: [fd validExtensions: exts].
	path ifNotNil: [fd selectPathName: path].
	^(aThemedMorph openModal: fd) answer