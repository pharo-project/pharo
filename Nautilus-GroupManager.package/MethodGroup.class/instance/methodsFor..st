queries
methodsFor: aClass
	| methodsForAClass |
	methodsForAClass := OrderedCollection new.
	aClass methods
		do: [ :method | 
			method methodClass = aClass
				ifTrue: [ methodsForAClass add: method ] ].
	^ methodsForAClass