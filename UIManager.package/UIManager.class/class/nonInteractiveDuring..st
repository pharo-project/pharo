utils
nonInteractiveDuring: aBlock
	| currentManager |
	currentManager := self default.
	currentManager nonInteractiveManager.
	
	aBlock ensure: [ currentManager beDefault ]