Extension { #name : #ProtocolOrganizer }

{ #category : #'*CodeExport' }
ProtocolOrganizer >> stringForFileOut [
	^ String
		streamContents: [ :aStream | 
			self protocols
				do: [ :p | 
					aStream << $( << p name printString.
					p methods do: [ :m | aStream << ' ' << m asString ].
					aStream
						<< $);
						newLine ] ]
]
