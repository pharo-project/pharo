keyboard control
removeMatchString
	"Remove the matchString, if any."
	self setProperty: #matchString toValue: String new.
	self displayFiltered: nil