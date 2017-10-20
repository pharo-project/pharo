private
withDefaultCharacterEncoderDo: block
	^ self defaultEncoder = ZnDefaultCharacterEncoder value
			ifTrue: block
			ifFalse: [ 
				ZnDefaultCharacterEncoder 
					value: self defaultEncoder  
					during: block ]