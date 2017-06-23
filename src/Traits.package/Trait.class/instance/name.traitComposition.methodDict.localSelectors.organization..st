initialize-release
name: aString traitComposition: aComposition methodDict: aMethodDict localSelectors: aSet organization: aClassOrganization

	"Used by copy"
	
	self name: aString.
	self localSelectors: aSet.
	self methodDict: aMethodDict.
	self traitComposition: aComposition.
	self organization: aClassOrganization