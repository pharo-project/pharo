stamp values
timeStamp 

	^ self annotationNamed: self class timeStampKey 
		ifAbsentPut: [ self class 
							parseTimestampFrom: self stamp 
							default: (DateAndTime epoch) ]