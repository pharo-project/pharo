pharo - updates
update60339
	"self new update60339"
	
Gofer new 
	smalltalkhubUser: 'Moose' project: 'Glamour';
	package: 'ConfigurationOfGlamourCore';
	load.
Gofer new 
	smalltalkhubUser: 'Moose' project: 'GToolkit';
	package: 'ConfigurationOfGToolkitCore';
	package: 'ConfigurationOfGTInspectorCore';
	package: 'ConfigurationOfGTDebugger';
	package: 'ConfigurationOfGTSpotter';
        package: 'ConfigurationOfGTEventRecorder';
	load.
	self withUpdateLog: '19497 Integrate GTools version 3.32
	https://pharo.fogbugz.com/f/cases/19497'.
	self loadTogether: self script60339 merge: false.
	self loadConfiguration: 'GToolkitCore' version: '3.32'.self flushCaches.
