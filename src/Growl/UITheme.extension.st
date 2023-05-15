Extension { #name : #UITheme }

{ #category : #'*Growl' }
UITheme >> growlBorderColorFor: aGrowlMorph [

	^ colorPalette growlBorderColorFor: aGrowlMorph
]

{ #category : #'*Growl' }
UITheme >> growlContentsColorFor: aGrowlMorph [

	^ colorPalette growlContentsColorFor: aGrowlMorph
]

{ #category : #'*Growl' }
UITheme >> growlDismissHandleFor: aGrowlMorph [
	| form image |
	form := self windowCloseForm.
	image := ImageMorph new.
	image image: form.
	image color: Color yellow.
	^ image
]

{ #category : #'*Growl' }
UITheme >> growlFillColorFor: aGrowlMorph [

	^ colorPalette growlFillColorFor: aGrowlMorph
]

{ #category : #'*Growl' }
UITheme >> growlLabelColorFor: aGrowlMorph [

	^ colorPalette growlLabelColorFor: aGrowlMorph
]
