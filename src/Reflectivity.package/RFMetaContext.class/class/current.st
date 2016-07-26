accessing
current 
	ActiveMetaContext value ifNil:[ActiveMetaContext value: self new].
	^ActiveMetaContext value
