morphic ui
morphicViewOnDirectory: aFileDirectory

	^ self 
		morphicViewOnFile: aFileDirectory 
		contents: nil
		fileList: (self new directory: aFileDirectory).