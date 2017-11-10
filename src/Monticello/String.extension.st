Extension { #name : #String }

{ #category : #'*monticello' }
String >> extractNumber [
	^ ('0', self select: [:ea | ea isDigit]) asNumber
]
