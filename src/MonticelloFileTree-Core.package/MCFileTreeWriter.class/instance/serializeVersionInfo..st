serializing
serializeVersionInfo: aVersionInfo
	infoWriter ifNil: [infoWriter := MCFileTreeVersionInfoWriter new].
	^ String streamContents: [:s |
		infoWriter stream: s.
		infoWriter writeVersionInfo: aVersionInfo]