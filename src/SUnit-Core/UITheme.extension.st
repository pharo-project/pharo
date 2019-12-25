Extension { #name : #UITheme }

{ #category : #'*SUnit-Core' }
UITheme >> testColorError [
	^ Color red darker darker 
]

{ #category : #'*SUnit-Core' }
UITheme >> testColorFailure [
	^ Color yellow darker darker darker
]

{ #category : #'*SUnit-Core' }
UITheme >> testColorPassing [
	^ Color green darker darker
]
