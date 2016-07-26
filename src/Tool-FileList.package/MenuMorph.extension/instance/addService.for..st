*Tool-FileList
addService: aService for: serviceUser
	"Append a menu item with the given service. If the item is selected, it will perform the given service."

	aService addServiceFor: serviceUser toMenu: self.