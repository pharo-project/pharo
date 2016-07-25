keyboard
flushKeyboard
	"Remove all characters from the keyboard buffer."

	[self keyboardPressed]
		whileTrue: [self keyboard]