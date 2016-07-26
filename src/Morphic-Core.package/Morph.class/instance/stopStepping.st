stepping and presenter
stopStepping
	"Stop getting sent the 'step' message."

	| w |
	w := self world.
	w ifNotNil: [w stopStepping: self].
