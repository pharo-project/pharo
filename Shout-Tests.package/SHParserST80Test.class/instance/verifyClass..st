utilities
verifyClass: aBehavior
	aBehavior selectors do: [ :selector | self verifyClass: aBehavior selector: selector ]
	