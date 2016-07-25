window animation
doAnimationFrameFor: window
	"TODO: Handle errors produced here"
	(OSWindowFrameEvent for: window) deliver