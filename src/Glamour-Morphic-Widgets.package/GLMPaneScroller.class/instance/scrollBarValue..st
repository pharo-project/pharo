updating
scrollBarValue: value
	transform hasSubmorphs ifFalse: [^ self].
	transform offset: (self leftoverScrollRange * value) rounded @ 0.