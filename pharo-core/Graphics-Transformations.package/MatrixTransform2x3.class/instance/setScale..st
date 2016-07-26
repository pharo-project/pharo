private
setScale: aPoint 
	"Set the raw scale in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a11: pt x asFloat.
	self a22: pt y asFloat