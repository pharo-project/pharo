instance creation
fromFileInfo: aFreeTypeFileInfo
	
	^self new
		fileInfo: aFreeTypeFileInfo;
		simulated: false;
		styleName: aFreeTypeFileInfo styleNameExtracted;
		stretchName: aFreeTypeFileInfo stretch;
		stretchValue: aFreeTypeFileInfo stretchValue;
		weightName: aFreeTypeFileInfo weight;
		weightValue: aFreeTypeFileInfo weightValue;
		slantName: aFreeTypeFileInfo slant;
		slantValue: aFreeTypeFileInfo slantValue;
		yourself
		