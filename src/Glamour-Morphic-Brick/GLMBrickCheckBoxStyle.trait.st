"
I'm a style module related to a checkbox and its componenets
"
Trait {
	#name : #GLMBrickCheckBoxStyle,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-checkbox' }
GLMBrickCheckBoxStyle >> checkboxBorderColor [

	^ Color lightGray
]

{ #category : #'brick-checkbox' }
GLMBrickCheckBoxStyle >> checkboxBorderWidth [

	^ 2
]

{ #category : #'brick-checkbox' }
GLMBrickCheckBoxStyle >> checkboxCheckedImage [

	^ GLMUIThemeExtraIcons glamorousAccept
]

{ #category : #'brick-checkbox' }
GLMBrickCheckBoxStyle >> checkboxHeight [

	^ 18
]

{ #category : #'brick-checkbox' }
GLMBrickCheckBoxStyle >> checkboxWidth [

	^ 18
]
