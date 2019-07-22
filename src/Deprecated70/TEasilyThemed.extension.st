Extension { #name : #TEasilyThemed }

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileOpen: title [
	"Answer the result of a file open dialog with the given title."
	self deprecated: 'Use UIManager default chooseExistingFileReference:extensions:path:preview:. instead.' on: '26 October 2018' in: #Pharo7.
	^self
		fileOpen: title
		extensions: nil
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileOpen: title extensions: exts [
	"Answer the result of a file open dialog with the given title and extensions to show."
	self deprecated: 'Use UIManager default chooseExistingFileReference:extensions:path:preview:. instead.' on: '26 October 2018' in: #Pharo7.
	^self
		fileOpen: title
		extensions: exts
		path: nil
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileOpen: title extensions: exts path: path [
	"Answer the result of a file open dialog with the given title, extensions to show and path."
	self deprecated: 'Use UIManager default chooseExistingFileReference:extensions:path:preview:. instead.' on: '26 October 2018' in: #Pharo7.
	^self 
		fileOpen: title
		extensions: exts
		path: path
		preview: nil
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileOpen: title extensions: exts path: path preview: preview [
	"Answer the result of a file open dialog with the given title, extensions to show, path and preview type."
	self deprecated: 'Use UIManager default chooseExistingFileReference:extensions:path:preview: instead.' on: '26 October 2018' in: #Pharo7.
	^self theme
		fileOpenIn: self
		title: title
		extensions: exts
		path: path
		preview: preview
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileSave: title [
	"Answer the result of a file save dialog with the given title."
	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path: instead.' on: '26 October 2018' in: #Pharo7.
	^self
		fileSave: title
		extensions: nil
		path: nil
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileSave: title extensions: exts [
	"Answer the result of a file save dialog with the given title."
	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path: instead.' on: '26 October 2018' in: #Pharo7.
	^self
		fileSave: title
		extensions: exts
		path: nil
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileSave: title extensions: exts path: path [
	"Answer the result of a file save dialog with the given title, extensions to show and path."
	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path: instead.' on: '26 October 2018' in: #Pharo7.
	^self theme
		fileSaveIn: self
		title: title
		extensions: exts
		path: path
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileSave: title initialAnswer: aFileName extensions: exts path: path [
	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path: instead.' on: '26 October 2018' in: #Pharo7.
	^self theme
		fileSaveIn: self
		title: title
		initialAnswer: aFileName
		extensions: exts
		path: path
]

{ #category : #'*Deprecated70' }
TEasilyThemed >> fileSave: title path: path [
	"Answer the result of a file save open dialog with the given title."
	self deprecated: 'Use UIManager default chooseForSaveFileReference:extensions:path: instead.' on: '26 October 2018' in: #Pharo7.
	^self
		fileSave: title
		extensions: nil
		path: path
]
