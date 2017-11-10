Extension { #name : #TextPresenter }

{ #category : #'*NodeNavigation' }
TextPresenter >> isWorkspace [
	^false
]

{ #category : #'*NodeNavigation' }
TextPresenter >> sourceTextArea [
	^ self widget
]
