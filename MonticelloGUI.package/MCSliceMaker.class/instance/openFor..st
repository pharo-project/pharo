user interface
openFor: anotherWindow
	window := self window.
	anotherWindow openModal: window.
	^ self resultInfo