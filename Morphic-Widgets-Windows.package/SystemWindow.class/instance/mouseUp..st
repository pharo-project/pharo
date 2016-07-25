events
mouseUp: evt
	| cp |
	cp := evt cursorPoint.
	submorphs do: [:m | (m containsPoint: cp) ifTrue: [m mouseUp: evt]]