Extension { #name : #SmalltalkEditor }

{ #category : #'*NodeNavigation' }
SmalltalkEditor >> isWorkspace [
	^ model isWorkspace
]

{ #category : #'*NodeNavigation' }
SmalltalkEditor >> sourceTextArea [
	^morph
]
