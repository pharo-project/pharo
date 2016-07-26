morphic ui
morphicViewOnFile: aFile
	|  window fileListBottom midLine fileListTopOffset buttonPane fileContentsView |

	window := (SystemWindow labelled: self labelString) model: self.

	fileListTopOffset := (TextStyle defaultFont pointSize * 2) + 11.
	fileListBottom := 0.4.
	midLine := 0.2.
	buttonPane := self optionalButtonRow addMorph: (self morphicPatternPane).
		
	self addFullPanesTo: window from: {
		{self morphicDirectoryTreePane. (0@0 corner: midLine@1) asLayoutFrame }.
		{buttonPane. (midLine@0 corner: 1@0) asLayoutFrame bottomOffset: (fileListTopOffset-3)}.
		{self morphicFileListPane. (midLine @ 0 corner: 1@fileListBottom) asLayoutFrame topOffset: fileListTopOffset}.
		{fileContentsView := self morphicFileContentsPane. (midLine@fileListBottom corner: 1@1) asLayoutFrame}.
	}.
	
	contents ifNotNil: [
		
		fileContentsView setText: contents asText.
		fileContentsView hasUnacceptedEdits: true].
	
	self postOpen.
	^ window 