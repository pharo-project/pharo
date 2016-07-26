adding
addAllSignatures
	
	self allPresentationMethods do: [ :aMethod | self addSignatureFor: aMethod ] 