interface
includesVersionNamed: aString
	"directly do a filename check since squeaksource only stores mcz"
	^ self includesFileNamed: aString, '.mcz'