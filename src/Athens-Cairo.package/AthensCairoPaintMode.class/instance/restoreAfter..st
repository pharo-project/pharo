convenience
restoreAfter: aBlock
	| op |
	op := self getOperator.
	
	aBlock ensure: [ self setOperator: op ]