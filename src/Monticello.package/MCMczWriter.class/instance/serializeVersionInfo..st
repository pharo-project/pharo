serializing
serializeVersionInfo: aVersionInfo
	infoWriter ifNil: [infoWriter := MCVersionInfoWriter new].
	^ String streamContents: [:s |
		infoWriter stream: s.
		infoWriter writeVersionInfo: aVersionInfo]