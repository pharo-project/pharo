accessing
globalNamed: aString 
	
	(self includesClassNamed: aString)
		ifTrue: [ ^ (self classNamed: aString) mirror ].
	self halt.