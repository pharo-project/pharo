instance creation
openOn: anURLString
	"
	self openOn: 'http://www.pharo.org'
	"
	| openCommand |
	openCommand := 'xdg-open {1} &' format: { anURLString }.
	self system: openCommand.