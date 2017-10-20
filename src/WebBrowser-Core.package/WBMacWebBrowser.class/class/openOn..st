instance creation
openOn: anURLString
	"
	self openOn: 'http://www.pharo.org'
	"
	| openCommand |
	openCommand := 'open "{1}"' format: { anURLString }.
	self system: openCommand.