services
fileSaveIn: aThemedMorph title: title initialAnswer: aFileName extensions: exts path: path
	"Answer the result of a file save dialog with the given title, extensions and path."

	|fd|
	fd := (FileDialogWindow newWithTheme: aThemedMorph theme)
		title: title;
		fileNameText: aFileName;
		answerSaveFile.
	exts ifNotNil: [fd validExtensions: exts].
	path ifNotNil: [fd selectPathName: path].
	^(aThemedMorph openModal: fd) answer