protected
setDefaultSerialization

	^ serializationFactory := [:anObject :anEncoder | 
		(FLDelayedSerializationMock 
			with: anEncoder
			root: anObject 
			analyzer: self analyzer)
			run;
			yourself ]