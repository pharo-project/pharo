registrations handling
sortRegistrations
	"Try to update MenuRegistration order inst. var and the sort the trees"
	self assignOrderWithBlock: self class orderAssignBlock.
	self sort: self class itemSortBlock