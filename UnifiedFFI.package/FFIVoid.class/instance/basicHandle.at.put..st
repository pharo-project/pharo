private
basicHandle: aHandle at: index put: value
	self error: 'You can not access plain void types (it is meaningless)'