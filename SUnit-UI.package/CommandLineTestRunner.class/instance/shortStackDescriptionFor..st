helper
shortStackDescriptionFor: anError
	^ (String streamContents: [ :s| 
			s << anError printString; cr.
			anError signalerContext shortDebugStackOn: s ])
		copyReplaceAll: String cr
		with: OSPlatform current lineEnding