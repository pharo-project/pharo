printing
writeExtraAttributesOn: aStream
	self extraAttributes
		associationsDo: [ :ea | 
			aStream
				nextPutAll: '; ';
				nextPutAll: ea key.
			(ea value isKindOf: Boolean)
				ifFalse: [ 
					aStream
						nextPutAll: '=';
						nextPutAll: ea value ] ]