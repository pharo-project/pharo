*Shout-Styling
shoutAboutToStyle: aPluggableShoutMorphOrView

	aPluggableShoutMorphOrView classOrMetaClass: self selectedClass.
	self selectedClass ifNil: [^ false].
	self selectedMethod ifNil: [^ false].
	^ true