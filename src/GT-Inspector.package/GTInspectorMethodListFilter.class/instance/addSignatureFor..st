adding
addSignatureFor: aMethod
	|signature|
	
	signature := self signatureFor: aMethod.
	(allMethodSignatures includes: signature) ifFalse: [ allMethodSignatures add: signature ].