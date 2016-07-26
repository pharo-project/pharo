accessing
toggleStickiness
	"togle the receiver's Stickiness"
	extension ifNil: [^ self beSticky].
	extension sticky: extension sticky not