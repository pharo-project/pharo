Extension { #name : #STONZnUrl }

{ #category : #'*STON-Core' }
STONZnUrl class >> fromSton: stonReader [
	^ ZnUrl fromString: stonReader parseListSingleton 
]

{ #category : #'*STON-Core' }
STONZnUrl class >> stonName [
	^ #URL
]
