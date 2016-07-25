private
a2forUrl: uri method: method
	^ self class md5Hash: method, ':', uri
	