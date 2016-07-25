tests
testEnvironmentOfOrganization
	"self debug: #testEnvironmentOfOrganization"
	
	| aDictionary |	
	aDictionary := SystemDictionary withOrganizer: SystemOrganizer new.
	self assert: aDictionary organization environment equals: aDictionary
	