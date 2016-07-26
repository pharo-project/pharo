accessing
eventHandler: anEventHandler 
	"Note that morphs can share eventHandlers and all is OK. "
	self assureExtension eventHandler: anEventHandler