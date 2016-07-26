instance creation
openWithLabel: aString contents: contentString color: aColor
	
	^ (self label: aString contents: contentString)
			backgroundColor: aColor;
			openInWorld