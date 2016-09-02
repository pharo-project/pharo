converting
asMouseMove
	"Convert the receiver into a mouse move"
	^MouseMoveEvent basicNew setType: #mouseMove startPoint: position endPoint: position trail: {position. position} buttons: buttons hand: source stamp: self timeStamp