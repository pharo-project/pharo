stepping and presenter
stopSteppingSelector: aSelector
	"Stop getting sent the given message."
	| w |
	w := self world.
	w ifNotNil: [w stopStepping: self selector: aSelector].
