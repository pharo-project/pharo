private
withMaximumEntitySizeDo: block
	^ self maximumEntitySize = ZnMaximumEntitySize value
			ifTrue: block
			ifFalse: [ 
				ZnMaximumEntitySize 
					value: self maximumEntitySize  
					during: block ]