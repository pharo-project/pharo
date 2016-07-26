handles
addCollapseHandle: handleSpec
	"Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand."

	| collapseHandle |
	(target owner notNil "nil happens, amazingly"
			and: [target owner isWorldOrHandMorph])
		ifFalse: [^ self].
	collapseHandle := self addHandle: handleSpec
		on: #mouseDown send: #mouseDownInCollapseHandle:with: to: self.
	collapseHandle on: #mouseUp send: #maybeCollapse:with: to: self.
	collapseHandle on: #mouseMove send: #setDismissColor:with: to: self
