instance creation
openOn: anURLString
	"
	self openOn: 'http://www.pharo.org'
	"
	^self shellExecute: 'open' file: anURLString parameters: '' directory: '' show: 5 "SW_SHOW"