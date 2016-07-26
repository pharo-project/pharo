private
closeSocketStream: socketStream
	super closeSocketStream: socketStream.
	self lock critical: [ 
		self connections remove: socketStream ifAbsent: [ ] ]