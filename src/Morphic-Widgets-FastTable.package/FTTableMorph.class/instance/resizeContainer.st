private
resizeContainer
	| topLeft bottomRight |
	topLeft := (self bounds left - self borderWidth) @ self bounds top - self borderWidth.
	bottomRight := (self bounds right - self verticalScrollBarWidth - self borderWidth) @ self bounds bottom - self borderWidth.
	self container
		bounds:
			(function isExplicite
				ifTrue: [ function resizeContainerFrom: topLeft to: bottomRight ]
				ifFalse: [ topLeft corner: bottomRight ])