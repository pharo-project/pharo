importing
readFrom: aStringOrStream pattern: pattern
	"See 
	DateParser comment"

	^ (DateParser readingFrom: aStringOrStream readStream pattern: pattern) parse
