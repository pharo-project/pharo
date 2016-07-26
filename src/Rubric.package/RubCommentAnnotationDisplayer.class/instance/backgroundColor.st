accessing
backgroundColor
	
	| clr |
	
	clr := self paragraphProvider backgroundColor.
	
	^clr luminance > 0.5 
		ifTrue: [ clr darker ]
		ifFalse: [ clr lighter]