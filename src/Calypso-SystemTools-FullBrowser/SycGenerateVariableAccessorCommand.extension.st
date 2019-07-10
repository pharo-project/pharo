Extension { #name : #SycGenerateVariableAccessorCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycGenerateVariableAccessorCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyVariableContextOfFullBrowser 
]
