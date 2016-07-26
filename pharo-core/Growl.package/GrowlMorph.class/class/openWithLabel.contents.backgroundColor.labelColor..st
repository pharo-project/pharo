instance creation
openWithLabel: aString contents: contentString backgroundColor: aColor labelColor: aLabelColor
	
	^ (self label: aString contents: contentString)
			backgroundColor: aColor;
			labelColor: aLabelColor;
			contentsColor: aLabelColor; 
			openInWorld