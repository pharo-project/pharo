Extension { #name : 'DebugSession' }

{ #category : '*Tool-Base' }
DebugSession >> implement: aMessage inClass: aClass forContext: aContext [
	| method |
	aClass compile: (DynamicMessageImplementor for: aMessage in: aClass) value.
	method := aClass lookupSelector: aMessage selector.

	MethodClassifier classify: method.

	aContext privRefreshWith: method.
	aContext method numArgs > 0 ifTrue:
		[aMessage arguments withIndexDo:
			[:arg :index|
				aContext tempAt: index put: arg]].

	self updateContextTo: aContext.
	self contextChanged.
	^ method
]
