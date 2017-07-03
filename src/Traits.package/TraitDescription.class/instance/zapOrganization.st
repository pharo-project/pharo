organization
zapOrganization
	"Remove the organization of this class by message categories.
	This is typically done to save space in small systems.  Classes and methods
	created or filed in subsequently will, nonetheless, be organized"

	self organization: nil.
	self isClassSide ifFalse: [self classSide zapOrganization]