*Polymorph-Widgets-Themes
setAsBackground
	"Set this form as a background image."
	| world |
	world := self currentWorld.
	world backgroundMorph: ((Smalltalk ui theme builder newAlphaImage: self help: nil) autoSize: false;
			 layout: #scaled;
			 lock) 