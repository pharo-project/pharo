events-accessing
updateableActionMap
	"Answer an updateable action map, saving it in my #actionMap property"
	
	| actionMap |
	self assureExtension.
	actionMap := extension actionMap.
	actionMap ifNil:
		[actionMap := self createActionMap.
		extension actionMap: actionMap].
	^actionMap