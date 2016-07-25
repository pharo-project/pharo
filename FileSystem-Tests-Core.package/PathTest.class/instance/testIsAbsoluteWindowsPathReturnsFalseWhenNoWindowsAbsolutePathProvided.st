tests
testIsAbsoluteWindowsPathReturnsFalseWhenNoWindowsAbsolutePathProvided
  
	self deny: (Path isAbsoluteWindowsPath: 'tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp/test').