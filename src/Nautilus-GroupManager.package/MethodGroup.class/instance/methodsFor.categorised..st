queries
methodsFor: aClass categorised: aSymbol
	| methodsForAClass |
	aSymbol ifNil: [ ^ self methodsFor: aClass ].
	methodsForAClass := OrderedCollection new.
	( aClass methodsInProtocol: aSymbol)
		do: [ :method | 
			method methodClass = aClass
				ifTrue: [ methodsForAClass add: method ] ].
	^ methodsForAClass