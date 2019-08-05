Extension { #name : #UITheme }

{ #category : #'*Deprecated70' }
UITheme >> fileOpenIn: aThemedMorph title: title extensions: exts path: path preview: preview [
	"Answer the result of a file open dialog with the given title, extensions path and preview type."

	| fd |
	self deprecated: 'Use UIManager default chooseExistingFileReference:extensions:path:preview:. instead.' on: '26 October 2018' in: #Pharo7.
	fd := FileDialogWindow basicNew
		basicTheme: aThemedMorph theme;
		previewType: preview;
		initialize;
		title: title;
		answerOpenFile.
	exts ifNotNil: [ fd validExtensions: exts ].
	path ifNotNil: [ fd selectPath: path ].
	^ (aThemedMorph openModal: fd) answer
]

{ #category : #'*Deprecated70' }
UITheme >> fileSaveIn: aThemedMorph title: title extensions: exts path: path [
	"Answer the result of a file save dialog with the given title, extensions and path."

	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path:preview:. instead.' on: '26 October 2018' in: #Pharo7.
	^self fileSaveIn: aThemedMorph title: title initialAnswer: '' extensions: exts path: path
]

{ #category : #'*Deprecated70' }
UITheme >> fileSaveIn: aThemedMorph title: title initialAnswer: aFileName extensions: exts path: path [
	"Answer the result of a file save dialog with the given title, extensions and path."

	| fd |
	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path:preview:. instead.' on: '26 October 2018' in: #Pharo7.
	fd := (FileDialogWindow newWithTheme: aThemedMorph theme)
		title: title;
		fileNameText: aFileName;
		answerSaveFile.
	exts ifNotNil: [ fd validExtensions: exts ].
	path ifNotNil: [ fd selectPath: path ].
	^ (aThemedMorph openModal: fd) answer
]

{ #category : #'*Deprecated70' }
UITheme >> taskbarButtonLabelColorForCollapsed: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is collapsed."

	self
		deprecated: 'Please use #taskbarItemLabelColorForCollapsed: instead'
		transformWith: '`@receiver taskbarButtonLabelColorForCollapsed: `@statements' -> '`@receiver taskbarItemLabelColorForCollapsed: `@statements'.
	^ self taskbarItemLabelColorForCollapsed: aButton
]

{ #category : #'*Deprecated70' }
UITheme >> taskbarButtonLabelColorForExpanded: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is expanded."

	self
		deprecated: 'Please use #taskbarItemLabelColorForExpanded: instead'
		transformWith: '`@receiver taskbarButtonLabelColorForExpanded: `@statements' -> '`@receiver taskbarItemLabelColorForExpanded: `@statements'.
	^ self taskbarItemLabelColorForExpanded: aButton
]
