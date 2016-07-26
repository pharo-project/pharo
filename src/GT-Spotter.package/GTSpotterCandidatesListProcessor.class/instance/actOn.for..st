private
actOn: anObject for: aStep
	
	actBlock
		ifNil: [ super actOn: anObject for: aStep ]
		ifNotNil: [ actBlock cull: anObject cull: aStep ]