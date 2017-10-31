accessing ficus classes
lookForFicusClass: aName
	^ self ficusPackage classes detect:[:e| e name = aName]