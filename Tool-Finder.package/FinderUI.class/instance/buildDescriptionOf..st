text areas behavior
buildDescriptionOf: aClass
	^ String
		streamContents:
			[ :stream | 
			stream nextPutAll: aClass definition.
			aClass hasComment
				ifTrue: [ stream
						cr;
						cr;
						cr;
						nextPutAll: aClass comment ] ]