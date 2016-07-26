geometry
buttonExtent
	| size |
	size := self theme scrollbarThickness.
	
	^ bounds isWide
		ifTrue: [ upButton 
			ifNil: [  size@size ] 
			ifNotNil: [ upButton extent ] ]
		ifFalse: [ downButton 
			ifNil: [ size@size ] 
			ifNotNil: [ downButton extent ] ]