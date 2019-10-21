Extension { #name : #ProtocolOrganizer }

{ #category : #'*CodeExport' }
ProtocolOrganizer >> stringForFileOut [
	^ String
		streamContents: [ :aStream | 
			self protocols
				do: [ :p | 
					aStream
						nextPut: $(;
						nextPutAll: p name printString.
					p methodSelectors
						do: [ :m | 
							aStream
								space;
								nextPutAll: m ].
					aStream
						nextPut: $);
						cr ] ]
]
