instance creation
show: aString from: startNumber to: endNumber
	self flag: 'When we go from Exceptions to announcements, delete this and use show:from:to:during:'.

	^ self uniqueInstance show: aString from: startNumber to: endNumber.