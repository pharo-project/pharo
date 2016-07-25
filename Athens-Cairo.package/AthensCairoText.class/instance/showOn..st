drawing
showOn: aCanvas
	| utf8String |
	aCanvas setPathMatrix.
	utf8String := self convertTextUtf8: self asString.
	self primShowText: utf8String on: aCanvas