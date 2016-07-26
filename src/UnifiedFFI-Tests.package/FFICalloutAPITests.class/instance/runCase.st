running
runCase 
	| oldAPIClass |
	
	"Ensure I use this class for running the tests"
	oldAPIClass := FFICalloutAPI calloutAPIClass.
	[ FFICalloutAPI calloutAPIClass: FFICalloutAPI. 
	  super runCase ]
	ensure: [ FFICalloutAPI calloutAPIClass: oldAPIClass ]