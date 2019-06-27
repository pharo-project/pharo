"
I'm a style module related to the button and its componenets
"
Trait {
	#name : #GLMBrickButtonStyle,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonBackgroundColor [

	^ Color veryVeryLightGray lighter
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonBorderColor [
	
	^ Color lightGray
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonBorderWidth [
	
	^ 1
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonMinHeight [

	^ 26 * self currentWorld displayScaleFactor
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonMinWidth [

	^ 70
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonPadding [

	^ #(3 7)
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonPressedColor [

	^ Color veryVeryLightGray lighter slightlyDarker slightlyDarker
]

{ #category : #'brick-button' }
GLMBrickButtonStyle >> buttonSelectedColor [

	^ Color veryVeryLightGray lighter slightlyDarker
]
