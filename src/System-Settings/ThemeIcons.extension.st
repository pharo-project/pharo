Extension { #name : #ThemeIcons }

{ #category : #'*System-Settings' }
ThemeIcons >> acceptSettings: aVisitor [
	^ aVisitor visitThemeIcons: self
]
