marker parsing
okToIgnoreMarker: aMarker

	^ (((16rE0 to: 16rEF) includes: aMarker) "unhandled APPn markers"
		or: [aMarker = 16rDC or: [aMarker = 16rFE]]) "DNL or COM markers"
		or: [aMarker = 16r99] "Whatever that is"