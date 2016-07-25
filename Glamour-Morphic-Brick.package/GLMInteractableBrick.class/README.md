A GLMInteractableBrick is a subclass of GLMBrick to allow user interactions such as: mouseLeave, mouseEnter, mouseDown, mouseUp and for each state can change background color or background image, that resizes accordinately to Brick's size. Supports doubleClick out of box, but it should be enabled sending enableDoubleClick message. When doubleClick is enabled announces #onDoubleClicked.

By default interactable brick is disabled and doesn't react to any user action. It should be first enabled sending enable message.

Instance Variables
	announcer:		<Announcer>
	checkedColor:		<Object>
	checkedImage:		<Object>
	isChecked:		<Boolean>
	isDoubleClickEnabled:		<Boolean>
	isEnabled:		<Boolean>
	isPressed:		<Boolean>
	isSelected:		<Boolean>
	normalColor:		<Object>
	normalImage:		<Object>
	pressedColor:		<Object>
	pressedImage:		<Object>
	selectedColor:		<Object>
	selectedImage:		<Object>

announcer
	- xxxxx

checkedColor
	- xxxxx

checkedImage
	- xxxxx

isChecked
	- xxxxx

isDoubleClickEnabled
	- xxxxx

isEnabled
	- xxxxx

isPressed
	- xxxxx

isSelected
	- xxxxx

normalColor
	- xxxxx

normalImage
	- xxxxx

pressedColor
	- xxxxx

pressedImage
	- xxxxx

selectedColor
	- xxxxx

selectedImage
	- xxxxx
