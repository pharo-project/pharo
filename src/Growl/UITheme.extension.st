Extension { #name : #UITheme }

{ #category : #'*Growl' }
UITheme >> growlBorderColorFor: aGrowlMorph [
	^ Color gray
]

{ #category : #'*Growl' }
UITheme >> growlContentsColorFor: aGrowlMorph [
	^ Color white
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
	^ Color darkGray alpha: 0.5
]

{ #category : #'*Growl' }
UITheme >> growlLabelColorFor: aGrowlMorph [
	^ Color white twiceDarker
]
