"
I'm a style module related to a rubric text fields and its componenets
"
Trait {
	#name : #GLMBrickRubricStyle,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricBackgroundColor [

	^ self contentBackgroundColor
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricBorderColor [

	^ Color transparent
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricBorderWidth [

	^ 0
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricCursorColor [

	^ self rubricTextColor
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricTextBackgroundColor [

	^ Color transparent
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricTextColor [

	^ self textColor
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricTextFamilyName [
	
	^ StandardFonts codeFont familyName
]

{ #category : #'brick-rubric' }
GLMBrickRubricStyle >> rubricTextFontSize [
	
	^ StandardFonts codeFont pointSize
]
