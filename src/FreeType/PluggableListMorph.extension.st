Extension { #name : #PluggableListMorph }

{ #category : #'*FreeType-override' }
PluggableListMorph >> setListFont [
	"set the font for the list"

	StandardFonts chooseFontWithTitle: 'Choose the font for this list' translated  for: self setSelector: #font:  getSelector: #font

]
