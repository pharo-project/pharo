private
setOffset: aPoint 
	"Set the raw offset in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat