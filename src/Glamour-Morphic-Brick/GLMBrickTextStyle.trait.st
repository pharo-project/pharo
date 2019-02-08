"
I'm a style module related to a string brick
"
Trait {
	#name : #GLMBrickTextStyle,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-text' }
GLMBrickTextStyle >> textFamilyName [

	^ self theme labelFont familyName
]

{ #category : #'brick-text' }
GLMBrickTextStyle >> textFontSize [

	^ self theme labelFont pointSize
]
