Extension { #name : #PharoDarkTheme }

{ #category : #'*DrTests' }
PharoDarkTheme >> testColorError [
	"Should be in SUnit-Core extention methods, but Sunit is used in the bootstrap."
	^ Color red darker darker darker
]

{ #category : #'*DrTests' }
PharoDarkTheme >> testColorFailure [
	"Should be in SUnit-Core extention methods, but Sunit is used in the bootstrap."
	^ Color yellow muchDarker lighter lighter
]

{ #category : #'*DrTests' }
PharoDarkTheme >> testColorPassing [
	"Should be in SUnit-Core extention methods, but Sunit is used in the bootstrap."
	^ Color green muchDarker lighter
]
