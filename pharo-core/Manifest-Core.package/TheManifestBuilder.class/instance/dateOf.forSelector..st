private
dateOf: ringobject forSelector: aSelector

	| critics |

	critics := self manifest perform: aSelector.
	^ ((critics detect: [:each | ((self browsedEnvironment at: each first first) manifestReadOn: (each first at: 2)) = ringobject ]) at: 2) asDateAndTime
		
		