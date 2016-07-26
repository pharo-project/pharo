shout
shoutAboutToStyle: aPluggableShoutMorph
	self highlightSmalltalk ifFalse: [^ false].
	self highlightSmalltalkContext ifNotNil: [ :classOrMetaClass |
 		aPluggableShoutMorph classOrMetaClass: classOrMetaClass  ].
	^ true