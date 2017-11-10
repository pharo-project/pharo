Extension { #name : #FinderUI }

{ #category : #'*Shout-Styling' }
FinderUI >> shoutAboutToStyle: aPluggableShoutMorphOrView [

	aPluggableShoutMorphOrView classOrMetaClass: self selectedClass.
	self selectedClass ifNil: [^ false].
	self selectedMethod ifNil: [^ false].
	^ true
]
