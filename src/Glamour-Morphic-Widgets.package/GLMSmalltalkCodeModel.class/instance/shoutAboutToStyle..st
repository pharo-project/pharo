accessing
shoutAboutToStyle: aPluggableShoutMorph
	self highlightSmalltalk ifFalse: [^ false].
	self highlightSmalltalkContext notNil ifTrue: [
		aPluggableShoutMorph classOrMetaClass: self highlightSmalltalkContext].
	^ true