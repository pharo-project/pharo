updating
update 
	self notingPresentationChangeDo: [
		"nothing in particular. 
		just make sure that matching presentations are recomputed"].
	self presentations do: [ :each | each update ]