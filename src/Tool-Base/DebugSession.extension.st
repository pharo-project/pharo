Extension { #name : 'DebugSession' }

{ #category : '*Tool-Base' }
DebugSession >> implement: aMessage inClass: aClass forContext: aContext [
	| method newContext |
	aClass compile: (DynamicMessageImplementor for: aMessage in: aClass) value.
	method := aClass lookupSelector: aMessage selector.

	MethodClassifier classify: method.

	newContext := Context newForMethod: method.
	newContext privRefreshWith: method.
	method numArgs > 0 ifTrue:
		[aMessage arguments withIndexDo:
			[:arg :index|
				newContext tempAt: index put: arg]].
	newContext sender: aContext sender.
	newContext receiver: aContext receiver.
	self updateContextTo: newContext.
	"Set the top context of the current process to the new created context"
	interruptedProcess suspendedContext: newContext.
	
	self contextChanged.
	^ method
]
