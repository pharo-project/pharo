comparing
= aRefactoryClassChange 
	self class = aRefactoryClassChange class
		ifFalse: [ ^ false ].
	^ className = aRefactoryClassChange changeClassName 
		and: [ isMeta = aRefactoryClassChange isMeta ]