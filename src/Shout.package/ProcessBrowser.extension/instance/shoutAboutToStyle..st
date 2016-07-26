*Shout-Styling
shoutAboutToStyle: aPluggableShoutMorphOrView

	selectedContext ifNil: [^false].
	aPluggableShoutMorphOrView classOrMetaClass: self selectedClass.
	^ true
