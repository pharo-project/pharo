tags
stable: spec
	"Symbolic versions can be used to introduce an indirection to a version number.
	The real name used by Metacello is only defined by the following pragma:"
	<symbolicVersion: #stable >
	
	"If another version is stable for a differen Smalltalk use a specific name"
	"spec for: #'pharo1.4.x' version: '0.9'"
	
	"Specfiy which exact version you want to load"
	spec for: #'common' version: '1.0'.