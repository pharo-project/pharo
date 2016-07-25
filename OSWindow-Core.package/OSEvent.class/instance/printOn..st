printing
printOn: aStream
	super printOn: aStream. 
	window ifNotNil: [ aStream << ' (WindowId=' << window windowId printString << ')' ]
