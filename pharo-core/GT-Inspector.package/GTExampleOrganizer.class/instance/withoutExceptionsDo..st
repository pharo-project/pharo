private
withoutExceptionsDo: aBlock
	[ aBlock value ]
		on: Error
		do: [ :exception | " we should not ignore it, but try to continue " ]