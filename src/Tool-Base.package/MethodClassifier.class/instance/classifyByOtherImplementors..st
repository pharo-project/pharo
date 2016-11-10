classification-rules
classifyByOtherImplementors: aMethod
	| protocolBag |
	
	protocolBag := Bag new.
	
	aMethod implementors 
		ifEmpty: [ ^ false ]
		ifNotEmpty: [ :methods |
			methods 
				do: [ :method | 
					((method protocol beginsWith: '*') or: [ method protocol = Protocol unclassified ])
						ifFalse: [ protocolBag add: method protocol ]]
				without: aMethod ].
			
	protocolBag ifEmpty: [ ^ false ].
	aMethod protocol: protocolBag sortedCounts first value.
	^ true