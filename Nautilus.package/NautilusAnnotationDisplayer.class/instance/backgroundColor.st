accessing
backgroundColor
	
	| clr |
	
	clr := self paragraphProvider color.
	
	^clr luminance > 0.5 
		ifTrue: [ clr darker ]
		ifFalse: [ clr lighter]