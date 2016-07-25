testing
testButtonMutex
	self assertButtonOn: 'instance'.
	self denyButtonOn: '?'.
	self denyButtonOn: 'class'.
	
	self clickOnButton: '?'.
	self assertButtonOn: '?'.
	self denyButtonOn: 'instance'.
	self denyButtonOn: 'class'.
	
	self clickOnButton: 'class'.
	self assertButtonOn: 'class'.
	self denyButtonOn: '?'.
	self denyButtonOn: 'instance'.
