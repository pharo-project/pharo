Extension { #name : #UITheme }

{ #category : #'*DrTests' }
UITheme >> testColorError [
	"Should be in SUnit-Core extention methods, but Sunit is used in the bootstrap."
	^ Color red darker darker 
]

{ #category : #'*DrTests' }
UITheme >> testColorFailure [
	"Should be in SUnit-Core extention methods, but Sunit is used in the bootstrap."
	^ Color yellow darker darker darker
]

{ #category : #'*DrTests' }
UITheme >> testColorPassing [
	"Should be in SUnit-Core extention methods, but Sunit is used in the bootstrap."
	^ Color green darker darker
]
