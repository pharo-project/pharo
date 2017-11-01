Class {
	#name : #GLMBrickDarkThemer,
	#superclass : #GLMBrickThemer,
	#category : 'Glamour-Morphic-Brick-Theme'
}

{ #category : #theme-actions }
GLMBrickDarkThemer >> actionThemer [

	^ GLMActionBrickDarkThemer new
]

{ #category : #brick-button }
GLMBrickDarkThemer >> buttonBackgroundColor [

	^ Color veryDarkGray lighter
]

{ #category : #brick-button }
GLMBrickDarkThemer >> buttonPressedColor [

	^ self backgroundColor whiter
]

{ #category : #brick-button }
GLMBrickDarkThemer >> buttonSelectedColor [

	^ Color veryDarkGray lighter slightlyDarker
]

{ #category : #brick-checkbox }
GLMBrickDarkThemer >> checkboxBorderColor [

	^ Color lightGray
]

{ #category : #brick }
GLMBrickDarkThemer >> contentBackgroundColor [

	^ Color darkGray darker
]

{ #category : #brick-scrollbar }
GLMBrickDarkThemer >> scrollbarColor [

	^ Color black alpha: 0.5
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickDarkThemer >> tabLabelThemer [

	^ self
		registerModule: GLMBrickTabLabelDarkThemer new
]

{ #category : #brick }
GLMBrickDarkThemer >> textColor [

	^ self color whiten
]

{ #category : #brick }
GLMBrickDarkThemer >> textSecondaryColor [

	^ Color white
]
