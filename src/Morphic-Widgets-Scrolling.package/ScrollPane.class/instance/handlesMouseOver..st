event handling
handlesMouseOver: evt
	"Could just ^ true, but this ensures that scroll bars won't flop out
	if you mouse-over appendages such as connecting pins."
	"self flag: #arNote." "I have no idea how the code below could've ever worked. If the receiver does not handle mouse over events then it should not receive any #mouseLeave if the mouse leaves the receiver for real. This is because 'evt cursorPoint' describes the *end* point of the movement and considering that the code would return false if the move ends outside the receiver the scroll bars should never pop back in again. Which is exactly what happens with the new event logic if you don't just ^true. I'm leaving the code in for reference - perhaps somebody can make sense from it; I sure cannot."
	^true
"
	| cp |
	cp := evt cursorPoint.
	(bounds containsPoint: cp)
		ifTrue: [^ true]			
		ifFalse: [self submorphsDo:
					[:m | (m containsPoint: cp) ifTrue:
							[m == scrollBar
								ifTrue: [^ true]
								ifFalse: [^ false]]].
				^ false]
"