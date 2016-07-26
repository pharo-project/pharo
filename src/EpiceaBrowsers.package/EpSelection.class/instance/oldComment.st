hooks
oldComment

	^ self items 
		ifEmpty: [ '' ]
		ifNotEmpty: [ :someItems | 
			someItems anyOne oldComment ]
	