private
basicHandle: aHandle at: index put: value
	self error: 'Not sure I want to do this.'
	"But it would be like this:
	aHandle pointerAt: index put: value"