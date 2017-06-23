*Iceberg-UI
description
	^ String streamContents: [ :stream | 
		stream 
			nextPutAll: self shortId; 
			nextPutAll: ' (';
			nextPutAll: self printShortTime;
			nextPut: $) ]
