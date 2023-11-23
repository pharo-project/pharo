Extension { #name : 'ZnUrl' }

{ #category : '*STON-Extensions' }
ZnUrl >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Extensions' }
ZnUrl >> stonOn: stonWriter [
	stonWriter
		writeObject: self
		named: STONZnUrl stonName
		listSingleton: self asString
]
