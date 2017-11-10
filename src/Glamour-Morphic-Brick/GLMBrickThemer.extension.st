Extension { #name : #GLMBrickThemer }

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> color [

	^ self
		registerModule: GLMBrickColorThemer new
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupEditorBorderThemer [

	^ self
		registerModule: GLMBrickPopupEditBorderThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupEditorShadowThemer [

	^ self
		registerModule: [ GLMBrickPopupEditShadowThemer new ]
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupErrorThemer [

	^ self
		registerModule: GLMBrickPopupErrorThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupHelpThemer [

	^ self
		registerModule: GLMBrickPopupHelpThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupSuccessThemer [

	^ self
		registerModule: GLMBrickPopupSuccessThemer new
		to: self
		named: #popupThemer
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> popupThemer [

	^ self
		registerModule: GLMBrickPopupThemer new
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrickThemer >> tabLabelThemer [

	^ self
		registerModule: GLMBrickTabLabelThemer new
]
