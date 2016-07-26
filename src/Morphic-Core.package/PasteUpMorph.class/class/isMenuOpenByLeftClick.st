acessing
isMenuOpenByLeftClick
	"Truth table:
	swap 	isMac	methodReturn
	0	 	0	 	1
	0	 	1	 	1
	1	 	0	 	0
	1	 	1 		1
"

	^ self shouldSwapMenuOpenerButtons ==> [ Smalltalk os isMacOS ]