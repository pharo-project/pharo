adding
addAndSelectSignatureFor: aMethod
	|signature|
	
	signature := self signatureFor: aMethod.
	(allMethodSignatures includes: signature) ifFalse: [ allMethodSignatures add: signature ].
	(selectedMethodSignatures includes: signature) ifFalse: [ selectedMethodSignatures add: signature ].
	