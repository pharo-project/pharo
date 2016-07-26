api
silentWhile: aBlock
	| oldSilent |
	
	oldSilent := silent.
	silent := true.
	aBlock value.
	silent := oldSilent