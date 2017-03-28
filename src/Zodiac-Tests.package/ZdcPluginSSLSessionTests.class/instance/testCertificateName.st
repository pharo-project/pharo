tests
testCertificateName
	
	self assert: session certificateName equals: nil.
	
	session certificateName: 'myCert.pem'.
	
	self assert: session certificateName equals: 'myCert.pem'.
	
	session certificateName: 'otherCert.pem'.
	
	self assert: session certificateName equals: 'otherCert.pem'.
	
	session certificateName: nil.
	
	self assert: session certificateName equals: nil.
