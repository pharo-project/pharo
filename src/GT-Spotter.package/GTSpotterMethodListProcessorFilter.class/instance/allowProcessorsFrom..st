adding
allowProcessorsFrom: aMethod
	|signature|
	
	signature := self signatureFor: aMethod.
	(selectedMethodSignatures includes: signature) ifFalse: [ selectedMethodSignatures add: signature ].
	