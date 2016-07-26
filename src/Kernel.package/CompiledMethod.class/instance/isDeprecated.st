testing
isDeprecated

	^ self sendsAnySelectorOf: #(
		#deprecated: 
		#deprecated:on:in:
		#deprecated:transformWith: 
	)