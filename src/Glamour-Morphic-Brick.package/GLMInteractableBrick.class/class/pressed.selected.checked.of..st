states
pressed: isPressed selected: isSelected checked: isChecked of: aTrick
	"answers sender with image, corresponding specified state"
	
	"pressed has the highest priority"
	(isPressed & aTrick pressedImage notNil)
		ifTrue: [ ^ aTrick pressedImage ].
	
	(isChecked & aTrick checkedImage notNil)
		ifTrue: [ ^ aTrick checkedImage ].
		
	(((isPressed & aTrick pressedImage isNil) | isSelected) & aTrick selectedImage notNil)
		ifTrue: [ ^ aTrick selectedImage ].
	
	^ aTrick normalImage.