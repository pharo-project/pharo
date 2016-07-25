brick-interactable-accessing
currentColor
	^ self state pressedColor: (self isPressed) selectedColor: (self isSelected) checkedColor: (self isChecked) of: self