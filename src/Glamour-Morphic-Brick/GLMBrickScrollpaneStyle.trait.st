"
I'm a style module related to a scrollpane and its component. 
"
Trait {
	#name : #GLMBrickScrollpaneStyle,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-scrollbar' }
GLMBrickScrollpaneStyle >> scrollbarColor [

	^ Color lightGray alpha: 0.5
]

{ #category : #'brick-scrollbar' }
GLMBrickScrollpaneStyle >> scrollbarCornerRadius [

	^ 0
]

{ #category : #'brick-scrollbar' }
GLMBrickScrollpaneStyle >> scrollbarOffset [

	^ 1
]

{ #category : #'brick-scrollbar' }
GLMBrickScrollpaneStyle >> scrollbarPosition [

	^ #right
]

{ #category : #'brick-scrollbar' }
GLMBrickScrollpaneStyle >> scrollbarWidth [

	^ 3
]

{ #category : #'brick-scrollpane' }
GLMBrickScrollpaneStyle >> scrollpaneBackgroundColor [

	^ self contentBackgroundColor
]
