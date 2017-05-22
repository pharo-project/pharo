actions
chooseReference
	^ (UIManager default 
		chooseFileName: self chooseTitle 
		extensions: nil 
		path: self location 
		preview: nil)
		ifNotNil: [ :file | file asFileReference  ]