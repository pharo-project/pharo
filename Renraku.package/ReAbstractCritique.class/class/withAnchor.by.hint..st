special instance creation
withAnchor: anAnchor by: aRule hint: aString

	^ (self
		withAnchor: anAnchor
		by: aRule)
		tinyHint: aString;
		yourself