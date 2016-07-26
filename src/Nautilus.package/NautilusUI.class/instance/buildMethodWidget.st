widget method
buildMethodWidget
	| methodsList |
	
	methodsList := methodWidget buildMethodsList.	
	
	self 
		setShortcuts: #NautilusMethodShortcuts 
		to: methodsList. 
	
	^methodsList	
