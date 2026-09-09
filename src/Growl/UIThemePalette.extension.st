Extension { #name : 'UIThemePalette' }

{ #category : '*Growl' }
UIThemePalette >> growlContentsColorFor: aGrowlMorph [

	^ paletteDictionary at: #growlContentsColorFor:
		  ifAbsentPut: [ colorConfigurator growlContentsColorFor: aGrowlMorph ]
]
