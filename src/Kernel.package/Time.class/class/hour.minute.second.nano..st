instance creation
hour: hour minute: minute second: second  nano: nano
 
	| nanoMultiplicative |
	
	nanoMultiplicative := 1000000000 / (10 ** nano numberOfDigits).	
	
 	^ self hour: hour minute: minute second: second nanoSecond: nano * nanoMultiplicative