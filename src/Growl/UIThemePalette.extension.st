Extension { #name : 'UIThemePalette' }

{ #category : '*Growl' }
UIThemePalette >> growlContentsColorFor: aGrowlMorph [

	^ self paletteDictionary at: #growlContentsColorFor: ifAbsentPut: [ self colorConfigurator growlContentsColorFor: aGrowlMorph ]
]
