Extension { #name : #UITheme }

{ #category : #'*growl' }
UITheme >> growlBorderColorFor: aGrowlMorph [
	^ Color gray
]

{ #category : #'*growl' }
UITheme >> growlContentsColorFor: aGrowlMorph [
	^ Color white
]

{ #category : #'*growl' }
UITheme >> growlDismissHandleFor: aGrowlMorph [
	| form image |
	form := self windowCloseForm.
	image := ImageMorph new.
	image image: form.
	image color: Color yellow.
	^ image
]

{ #category : #'*growl' }
UITheme >> growlFillColorFor: aGrowlMorph [
	^ Color darkGray alpha: 0.5
]

{ #category : #'*growl' }
UITheme >> growlLabelColorFor: aGrowlMorph [
	^ Color white twiceDarker
]
