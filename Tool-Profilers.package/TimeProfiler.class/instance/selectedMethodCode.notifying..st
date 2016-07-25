compiling
selectedMethodCode: aString notifying: aController
	"Compile the code in aString. Notify aController of any syntax errors. 
	Answer false if the compilation fails. Otherwise answer true."

	self selectedNode
		ifNil: [ ^ false ]
		ifNotNil: [ :currNode | 
			| class oldSelector category newSelector |
			class := currNode methodClass.
			oldSelector := currNode selector.
			class ifNil: [ ^ false ].	"Normal method accept"
			category := class organization categoryOfElement: oldSelector.
			newSelector := class compile: aString classified: category notifying: aController.
			newSelector isNil
				ifTrue: [ ^ false ].
			self changed: #selectedMethodCode.
			^ true ]