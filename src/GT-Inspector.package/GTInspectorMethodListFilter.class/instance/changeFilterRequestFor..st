accessing
changeFilterRequestFor: aGTInspector

	self
		openListChooseLabeled: 'All available presentations' 
		forInspector: aGTInspector
		with: allMethodSignatures 
		select: selectedMethodSignatures 
		format: [ :aSignature | self printSignature: aSignature ]
		actionSelector: #replaceSelectedSignaturesWith:in: