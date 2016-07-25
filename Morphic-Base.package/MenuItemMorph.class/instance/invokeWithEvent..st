events
invokeWithEvent: evt
	"Perform the action associated with the given menu item."

	| w |
	self isEnabled ifFalse: [^ self].
	owner ifNotNil:[self isStayUpItem ifFalse:[
		self flag: #workAround. "The tile system invokes menus straightforwardly so the menu might not be in the world."
		(w := self world) ifNotNil:[
			owner deleteIfPopUp: evt.
			"Repair damage before invoking the action for better feedback"
			w displayWorldSafely]]].
	selector ifNil:[^self].
	Cursor normal showWhile: [ | selArgCount |  "show cursor in case item opens a new MVC window"
		(selArgCount := selector numArgs) = 0
			ifTrue:
				[target perform: selector]
			ifFalse:
				[selArgCount = arguments size
					ifTrue: [target perform: selector withArguments: arguments]
					ifFalse: [target perform: selector withArguments: (arguments copyWith: evt)].
		self showShortcut ].
		self changed].