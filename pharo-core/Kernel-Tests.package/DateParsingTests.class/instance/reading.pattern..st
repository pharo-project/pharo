asserting
reading: aPattern pattern: aString
	^ (self dateParserClass readingFrom: aString readStream pattern: aPattern) parse