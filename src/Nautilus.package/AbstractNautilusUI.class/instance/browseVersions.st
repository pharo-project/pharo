menus behavior
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	self selectedMethod ifNil: [ ^ self inform: 'No method selected' ].
	self browseVersionsFrom: self selectedMethod