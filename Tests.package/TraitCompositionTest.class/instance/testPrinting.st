testing-basic
testPrinting
	| composition1 composition2 |
	composition1 := ((self t2 + self t1) @ { (#z -> #c) } - {#a. #b. #c } 
				+ self t3 - { #d. #e } 
				+ self t4).
	composition2 := self t4 @ { (#x -> #a). (#y -> #b) } + self t1 - { #a } 
				+ self t3 - { #d. #e } 
				+ self t2 - { #b. #c }.
	self assertPrints: composition1 printString
		like: 'T2 @ {#z->#c} - {#d. #e. #a. #b. #c} + (T1 @ {#z->#c} - {#d. #e. #a. #b. #c}) + (T3 - {#d. #e}) + T4'.
	self assertPrints: composition2 printString
		like: 'T4 @ {#x->#a. #y->#b} - {#b. #c. #d. #e. #a} + (T1 - {#b. #c. #d. #e. #a}) + (T3 - {#b. #c. #d. #e}) + (T2 - {#b. #c})'
		
		