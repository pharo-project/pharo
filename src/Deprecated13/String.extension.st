Extension { #name : 'String' }

{ #category : '*Deprecated13' }
String >> asSignedInteger [
	"Returns the first signed integer it can find or nil."

	self
		deprecated:
		'Use #asInteger to convert to a valid integer. If you want to extract the integer part, use #extractIntegerPart'
		transformWith:
		'`@receiver asSignedInteger' -> '`@receiver extractIntegerPart'.
	^ self extractIntegerPart
]
