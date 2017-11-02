Extension { #name : #TimeProfiler }

{ #category : #'*Shout-Styling' }
TimeProfiler >> shoutAboutToStyle: aPluggableShoutMorphOrView [ 
	self selectedNode
		ifNotNil: [ :currNode | 
			aPluggableShoutMorphOrView getTextSelector = #selectedMethodCode
				ifTrue: [aPluggableShoutMorphOrView classOrMetaClass: currNode methodClass.
					^ true]].
	aPluggableShoutMorphOrView getTextSelector = #summaryText
			ifTrue: [^ false].
	aPluggableShoutMorphOrView getTextSelector = #blockCode
			ifTrue: [^ true].
	 ^ false

]
