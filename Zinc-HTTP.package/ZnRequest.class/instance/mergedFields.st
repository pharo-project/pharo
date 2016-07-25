accessing
mergedFields
	"Return a new ZnMultiValueDictionary containing all query fields, if any,
	merged with all ZnApplicationFormUrlEncodedEntity fields, if any,
	preserving multi values for identical keys"

	| fields |
	fields := ZnMultiValueDictionary new.
	self uri hasQuery
		ifTrue: [ fields addAllMulti: self uri query ].
	(self contentType = ZnMimeType applicationFormUrlEncoded and: [ self hasEntity ])
		ifTrue: [ fields addAllMulti: self entity fields ].
	^ fields