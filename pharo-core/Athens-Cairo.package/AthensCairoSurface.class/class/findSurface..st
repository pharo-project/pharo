private
findSurface: surfaceId
	| buf |
	buf := ByteArray new: 4.  "sizeof(int)"
	
	
	(self ioFindSurface: surfaceId handle: buf) ifTrue: [
		^ buf nbUlongAt: 1
		].
	
	^ nil. 