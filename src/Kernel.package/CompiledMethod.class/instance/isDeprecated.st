testing
isDeprecated

	^ self sendsAnySelectorOf: #(
		#deprecated: 
		#deprecated:on:in:
		#deprecated:on:in:transformWith:
		#deprecated:transformWith: 
	)