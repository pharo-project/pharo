brick-interactable-accessing
currentImage
	^ self state pressed: (self isPressed) selected: (self isSelected) checked: (self isChecked) of: self