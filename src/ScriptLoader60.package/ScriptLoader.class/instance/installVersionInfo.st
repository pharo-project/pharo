fixing stream
installVersionInfo
	"self new installVersionInfo"

	| highestUpdate newVersion |
	highestUpdate := SystemVersion current highestUpdate.
	(self confirm: 'Reset highest update (' , highestUpdate printString , ')?')
		ifTrue: [SystemVersion current highestUpdate: 0].

	newVersion := UIManager default request: 'New version designation:' translated initialAnswer: '3.9' , highestUpdate printString. 
	SystemVersion newVersion: newVersion.
	
