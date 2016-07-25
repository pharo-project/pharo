services
chooseFileNameIn: aThemedMorph title: title patterns: patterns path: path preview: preview
	"Answer the result of a file name chooser dialog with the given title, patterns
	path and preview type.
	Answer nil or a filename."

	|fd|
	fd := FileDialogWindow basicNew
		basicTheme: aThemedMorph theme;
		previewType: preview;
		initialize;
		title: title;
		answerFileName.
	patterns ifNotNil: [
		fd fileSelectionBlock: [:de |
			(fd defaultFileSelectionBlock value: de) and: [
				de isDirectory or: [patterns anySatisfy: [:pat | pat match: de name]]]]].
	path ifNotNil: [fd selectPathName: path].
	^(aThemedMorph openModal: fd) answer