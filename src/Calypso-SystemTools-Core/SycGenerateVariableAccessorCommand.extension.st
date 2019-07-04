Extension { #name : #SycGenerateVariableAccessorCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycGenerateVariableAccessorCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemFor: ClySourceCodeContext
]
