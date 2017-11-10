Extension { #name : #AbstractTool }

{ #category : #'*SmartSuggestions' }
AbstractTool >> browseAllStoresInto: aVariableName from: aClass [ 
	^self systemNavigation browseAllStoresInto: aVariableName from: aClass 
]
