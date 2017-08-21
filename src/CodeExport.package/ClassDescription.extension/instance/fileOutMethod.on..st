*CodeExport
fileOutMethod: selector on: aStream
	
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	aStream header; timeStamp.
	self printMethodChunk: selector on: aStream.