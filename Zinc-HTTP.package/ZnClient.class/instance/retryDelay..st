options
retryDelay: seconds
	"Set the delay before I will delay retrying a failed request to seconds,
	provided I do retry requests."

	self optionAt: #retryDelay put: seconds