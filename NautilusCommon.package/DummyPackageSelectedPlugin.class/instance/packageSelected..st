announcement
packageSelected: anAnnouncement
	
	| package name |
	package := anAnnouncement package.
	name := package name asString.
	package ifNil: [ name := ''].
	morph ifNotNil: [ morph contents: name]