private
servicesForFileNamed: aString 
	"private - answer a collection of file-services for the file named  
	aString"
	| allServices |
	allServices := FileServices itemsForFile: aString.
	^ allServices
		reject: [:svc | self unwantedSelectors includes: svc selector]