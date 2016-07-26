adding
addAndSelectAllSignatures
	
	self allPresentationMethods do: [ :aMethod | self addAndSelectSignatureFor: aMethod ] 