pharo - updates
update60436
	"self new update60436"
	
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
	self withUpdateLog: '19812 GTools 3.36
	https://pharo.fogbugz.com/f/cases/19812'.
	self loadTogether: self script60436 merge: false.
	self loadConfiguration: 'GToolkitCore' version: '3.36'.self flushCaches.
