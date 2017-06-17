actions
chooseReference
	^ UIManager default 
		chooseDirectory: self chooseTitle 
		path: self location