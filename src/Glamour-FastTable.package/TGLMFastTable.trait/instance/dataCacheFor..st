cache
dataCacheFor: anElement.

	^ self dataCache at: anElement ifAbsentPut: [ Dictionary new ]