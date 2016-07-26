private
silentWhile: aBlock
	| oldSilent |
	
	oldSilent := silent.
	silent := true.
	aBlock ensure: [ silent := oldSilent ]