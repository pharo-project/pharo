accessing
disableFunction
	"Disabling it just sets the funtion to nil, so I can safely skip it in #keyStrokeSearch:"

	function disable.
	self resizeAllSubviews.	"This is call because now the container will take all the available space."
	function := FTNilFunction table: self