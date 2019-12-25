Extension { #name : #PharoDarkTheme }

{ #category : #'*SUnit-Core' }
PharoDarkTheme >> testColorError [
	^ Color red darker darker darker
]

{ #category : #'*SUnit-Core' }
PharoDarkTheme >> testColorFailure [
	^ Color yellow muchDarker lighter lighter
]

{ #category : #'*SUnit-Core' }
PharoDarkTheme >> testColorPassing [
	^ Color green muchDarker lighter
]
