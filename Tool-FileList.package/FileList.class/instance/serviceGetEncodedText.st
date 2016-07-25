own services
serviceGetEncodedText

	^  (SimpleServiceEntry 
			provider: self 
			label: 'View as encoded text'
			selector: #getEncodedText
			description: 'View as encoded text')

