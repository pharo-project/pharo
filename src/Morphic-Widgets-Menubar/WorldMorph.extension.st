Extension { #name : #WorldMorph }

{ #category : #'*Morphic-Widgets-Menubar' }
WorldMorph >> menubar [

	^ self submorphs detect: #isMenubar
]
