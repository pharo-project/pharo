private
headerIconChanged
	
	self widgetDo: [ :w | 
		self headerIcon
			ifNil: [ w header removeIcon ]
			ifNotNil: [ w header icon: (ImageMorph new form: self headerIcon) ] ]