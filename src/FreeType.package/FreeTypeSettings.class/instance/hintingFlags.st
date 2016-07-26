accessing
hintingFlags
	| answer |
	answer := 0. 	
	self hinting 
		ifTrue:[
			self forceAutoHinting ifTrue:[answer := answer bitOr: 32 "forceAutoHinting"].	
			self lightHinting ifTrue:[answer := answer bitOr: LoadTargetLight].
			self monoHinting ifTrue:[answer := answer bitOr: LoadTargetMono].
			self lcdHinting ifTrue:[answer := answer bitOr: LoadTargetLCD].
			self lcdvHinting ifTrue:[answer := answer bitOr: LoadTargetLCDV]]
		ifFalse:[answer := 2 "no hinting"].
	^answer