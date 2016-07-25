morphic ui
morphicViewOnFile: aFile contents: contents fileList: aFileList
	|  window fileListBottom midLine fileListTopOffset buttonPane fileContentsView |

	window := (SystemWindow labelled: aFile fullName) model: aFileList.

	fileListTopOffset := (TextStyle defaultFont pointSize * 2) + 11.
	fileListBottom := 0.4.
	midLine := 0.2.
	buttonPane := aFileList optionalButtonRow addMorph:
		(aFileList morphicPatternPane).
		
	self addFullPanesTo: window from: {
		{aFileList morphicDirectoryTreePane. (0@0 corner: midLine@1) asLayoutFrame }.
		{buttonPane. (midLine@0 corner: 1@0) asLayoutFrame bottomOffset: (fileListTopOffset-3)}.
		{aFileList morphicFileListPane. (midLine @ 0 corner: 1@fileListBottom) asLayoutFrame topOffset: fileListTopOffset}.
		{fileContentsView := aFileList morphicFileContentsPane. (midLine@fileListBottom corner: 1@1) asLayoutFrame}.
	}.
	
	contents ifNotNil: [
		fileContentsView editString: contents.
		fileContentsView hasUnacceptedEdits: true].
	
	aFileList postOpen.
	^ window 