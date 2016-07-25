menu - change set
subtractFrom: src to: dest
	"Subtract the changes found on the other side from the requesting side."
	| source destination |
	source := src selectedChangeSet.
	destination := dest selectedChangeSet.
	
	self checkThatSidesDiffer: [^ self].
	
	self model subtractFrom: source to: destination.
	
	changeSorterRight setSelectedChangeSet: source.