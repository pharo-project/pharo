Extension { #name : #DiffMorph }

{ #category : #'*Shout-Styling' }
DiffMorph >> shoutAboutToStyle: aPluggableShoutMorphOrView [
	aPluggableShoutMorphOrView classOrMetaClass: self contextClass.
	^ self contextClass notNil 
]
