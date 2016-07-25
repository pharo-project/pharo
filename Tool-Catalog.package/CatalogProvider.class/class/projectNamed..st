accessing
projectNamed: projectName
	"Lookup and return a particular project in the catalog, else fail with NotFound"
	
	"self projectNamed: 'ZTimestamp'"
	
	^ self default projects 
		detect: [ :each | each name = projectName ]
		ifNone: [ NotFound signalFor: projectName in: self default ]