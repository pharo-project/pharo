menus
loadMethodSelection
	"Install the selected change"
	
	self selectedChange ifNil: [ ^self ].
	self selectedChange definition load.