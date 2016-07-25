private
privateStyle: aText 
	(self rangesIn: aText setWorkspace: true)
		ifNotNil: [ :ranges| self setAttributesIn: aText fromRanges: ranges].
	^aText.