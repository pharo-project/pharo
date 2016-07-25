own services
serviceGetHex

	^  (SimpleServiceEntry 
			provider: self 
			label: 'View as hex' 
			selector: #getHex
			description: 'View as hex')
			
