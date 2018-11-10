Extension { #name : #DateAndTime }

{ #category : #'*Iceberg' }
DateAndTime >> asLocalStringYMDHM [
	^ self asLocal asStringYMDHM
]

{ #category : #'*Iceberg' }
DateAndTime >> asStringYMDHM [
	^ String streamContents: [ :aStream |
		self printYMDOn: aStream.
		aStream nextPut: Character space.
		self hour printOn: aStream base: 10 length: 2 padded: true.
		aStream nextPut: $:.
		self minute printOn: aStream base: 10 length: 2 padded: true ]
]
