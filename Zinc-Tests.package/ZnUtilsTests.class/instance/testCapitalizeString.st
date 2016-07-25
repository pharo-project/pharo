testing
testCapitalizeString
	self assert: (ZnUtils capitalizeString: 'content-type') = 'Content-Type'.
	self assert: (ZnUtils capitalizeString: 'CONTENT-type') = 'Content-Type'.
	self assert: (ZnUtils capitalizeString: 'content-TYPE') = 'Content-Type'.
	self assert: (ZnUtils capitalizeString: 'WWW-Authenticate') = 'Www-Authenticate'.
	self assert: (ZnUtils capitalizeString: 'Content-MD5') = 'Content-Md5'.