initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@object keys do: ``@block' with: '``@object keysDo: ``@block';
		replace: '``@object values do: ``@block' with: '``@object valuesDo: ``@block'
