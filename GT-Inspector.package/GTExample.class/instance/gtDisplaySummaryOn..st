ui
gtDisplaySummaryOn: aStream.
	self label isEmptyOrNil 
		ifFalse: [ ^ aStream nextPutAll: self label ].
	self description isEmptyOrNil 
		ifFalse: [ ^ aStream nextPutAll: self description ].
	aStream nextPutAll: self method printString