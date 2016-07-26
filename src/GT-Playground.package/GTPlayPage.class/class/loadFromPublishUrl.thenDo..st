instance creation
loadFromPublishUrl: aString thenDo: aBlock
	| page |
	page := self loadFromPublishUrl: aString.
	page ifNotNil: [ aBlock value: page ].
	^ page