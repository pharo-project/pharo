initialize-release
readEntityFrom: stream
	(self isInformational or: [ self isNoContent or: [ self isNotModified ] ])
		ifFalse: [ super readEntityFrom: stream ]