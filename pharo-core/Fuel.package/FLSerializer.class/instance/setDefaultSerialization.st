protected
setDefaultSerialization

	^ serializationFactory := [:anObject :anEncoder | 
		(FLSerialization 
			with: anEncoder
			root: anObject 
			analyzer: self analyzer)
			run;
			yourself ]