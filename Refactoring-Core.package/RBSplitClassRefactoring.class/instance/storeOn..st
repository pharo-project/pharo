printing
storeOn: aStream 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' instanceVariables: '.
	instanceVariables asArray storeOn: aStream.
	aStream
		nextPutAll: ' newClassName: #';
		nextPutAll: newClassName;
		nextPutAll: ' referenceVariableName: ''';
		nextPutAll: referenceVariableName;
		nextPutAll: ''')'