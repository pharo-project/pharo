*renraku
match: aNode onSuccess: aBlock onFailure: anAlternativeBlock 
	| context |
	
	context := SmallDictionary new.
	
	(self match: aNode inContext: context)
		ifTrue: [ aBlock cull: context ]
		ifFalse: anAlternativeBlock