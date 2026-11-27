Extension { #name : 'SolidFillStyle' }

{ #category : '*Morphic-Halos' }
SolidFillStyle >> changeColorIn: aMorph event: evt [
	"Note: This is just a workaround to make sure we don't use the old color inst var"
	aMorph changeColorTarget: aMorph selector: #color: originalColor: self asColor hand: evt hand
]
