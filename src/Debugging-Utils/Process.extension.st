Extension { #name : 'Process' }

{ #category : '*Debugging-Utils' }
Process >> completeTo: aContext [
	"Resume self until aContext is on top"

	self suspendedContext == aContext ifTrue: [^ aContext].
	^ self complete: (self calleeOf: aContext)
]
