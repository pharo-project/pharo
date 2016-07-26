accessing
pane: aPane 
	super pane: aPane.
	self presentations do: [:each | each pane: aPane ]