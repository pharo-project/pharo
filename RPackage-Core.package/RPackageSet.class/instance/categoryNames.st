accessing
categoryNames
	^  self packages 	
		inject:  #()
		into: [ :all :each | all, (each classTagCategoryNames asArray) ]