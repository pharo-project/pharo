spec accessing
spec
	"return my PluggableMenuItemSpec"
	^ spec ifNil: [spec := PluggableMenuItemSpec new]