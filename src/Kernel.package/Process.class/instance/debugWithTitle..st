debugging
debugWithTitle: title
	"Open debugger on self"

	| context |
	context := self isActiveProcess ifTrue: [thisContext] ifFalse: [self suspendedContext].
	^ self debug: context title: title full: true.
