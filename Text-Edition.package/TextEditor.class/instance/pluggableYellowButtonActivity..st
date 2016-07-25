pluggable menus
pluggableYellowButtonActivity: shiftKeyState
	"Invoke the model's popup menu."

	| menu |
	(menu := self getPluggableYellowButtonMenu: shiftKeyState)
		ifNil: [
			^ false]
		ifNotNil: [
			menu invokeOn: model orSendTo: self.
			^ true]