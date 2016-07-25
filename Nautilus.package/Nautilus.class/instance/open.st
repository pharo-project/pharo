displaying
open
	ui := (NautilusUI on: self) open.
	self announce: (WindowOpened new window: ui window)