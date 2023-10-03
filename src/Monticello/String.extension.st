Extension { #name : 'String' }

{ #category : '*Monticello' }
String >> extractNumber [
	^ ('0', self select: [:ea | ea isDigit]) asNumber
]
