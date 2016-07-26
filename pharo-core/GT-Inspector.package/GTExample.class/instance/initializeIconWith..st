private
initializeIconWith: aBlock
	<todo: 'come up with something better than an empty & generic exception handler'>
	" We don't want to throw exceptions while examples are being created. We regard those as optional / nice-to-have. "
	[ aBlock value ]
		on: Error
		do: [ :exception | ]