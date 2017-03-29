pharo - updates
update60454
	"self new update60454"
	
Gofer new 
	smalltalkhubUser: 'Moose' project: 'Glamour';
	package: 'ConfigurationOfGlamourCore';
	load.
Gofer new 
	smalltalkhubUser: 'Moose' project: 'GToolkit';
	package: 'ConfigurationOfGTInspectorCore';
	package: 'ConfigurationOfGTDebugger';
	package: 'ConfigurationOfGTSpotter';
        package: 'ConfigurationOfGTEventRecorder';
	load.
	self withUpdateLog: '19878 GToolkit 3.37
	https://pharo.fogbugz.com/f/cases/19878'.
	self loadTogether: self script60454 merge: false.
	self loadConfiguration: 'GToolkitCore' version: '3.37'.self flushCaches.
