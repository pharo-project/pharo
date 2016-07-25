tests
testObjectIsNotAbstract
	self deny: objectClass isAbstract.
	self deny: objectClass theMetaClass isAbstract.