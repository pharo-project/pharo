comparing
= anObject
	^ self == anObject or: [
		self class == anObject class and: [ 
			self isReady and: [ 
				anObject isReady and: [ 
					self compare: anObject ] ] ] ]