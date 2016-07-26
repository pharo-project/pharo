classification-rules
classifyByOtherImplementors: aMethod
	| protocolBag |
	
	protocolBag := Bag new.
	
	aMethod implementors 
		ifEmpty: [ ^ false ]
		ifNotEmpty: [ :methods |
			methods 
				do: [ :method | 
					self flag: 'TODO: at some point we should have first-class protocols which will tell whether they are extensions...'.
					((method protocol beginsWith: '*') or: [ method protocol = Protocol unclassified ])
						ifFalse: [ protocolBag add: method protocol ]]
				without: aMethod ].
			
	protocolBag ifEmpty: [ ^ false ].
	aMethod protocol: protocolBag sortedCounts first value.
	^ true