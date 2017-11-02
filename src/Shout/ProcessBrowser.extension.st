Extension { #name : #ProcessBrowser }

{ #category : #'*Shout-Styling' }
ProcessBrowser >> shoutAboutToStyle: aPluggableShoutMorphOrView [

	selectedContext ifNil: [^false].
	aPluggableShoutMorphOrView classOrMetaClass: self selectedClass.
	^ true

]
