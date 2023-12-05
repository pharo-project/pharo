Extension { #name : 'Metaclass' }

{ #category : '*ClassDefinitionPrinters' }
Metaclass >> definitionStringFor: aConfiguredPrinter [

	^ aConfiguredPrinter metaclassDefinitionString
]
