private dispatch
registerSelectionGroup: aSelection
	self model selectedGroup: aSelection item.
	
	self flag: #todo. "I need to refactor to remove the need of groups"
	"I select also group-as-a-package, because I will be filtering with package selection, 
	 not with group selection (which actually does not has much sense now)"
	self model 	
		package: aSelection
		class: self selectedClass.
	
	self packageWidget selectGroup: aSelection item.
