Extension { #name : #ThemeIcons }

{ #category : #'*System-Settings-Core' }
ThemeIcons >> acceptSettings: aVisitor [
	^ aVisitor visitThemeIcons: self
]
