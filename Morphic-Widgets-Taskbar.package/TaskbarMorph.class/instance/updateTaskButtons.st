private
updateTaskButtons
	"Make buttons for the ordered tasks."
	
	| oldButtons size |
	oldButtons := self submorphs copy.
	self removeAllMorphs.
	self defer: [oldButtons do: [:b | b model: nil]]. "release dependency after event handling"
	
	size := self orderedTasks size.
	(self orderedTasks copyFrom: ( size - self class maximumButtons + 1 max: 1 ) to: size) do: [:t | | button |
		button := t taskbarButtonFor: self.
		button ifNotNil: [self addMorphBack: button]]