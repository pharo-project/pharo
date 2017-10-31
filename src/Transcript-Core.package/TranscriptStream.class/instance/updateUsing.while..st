updating
updateUsing: aStrategy while: aBlock

	| currentStrategy |
	
	currentStrategy := updateStrategy.	
	updateStrategy := aStrategy .
	
	aBlock ensure: [ updateStrategy := currentStrategy ]. 