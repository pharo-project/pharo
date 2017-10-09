accessing
directoryForPlatform 
	| dir |
	"I don't know how to do this in other platforms (not Pharo)"
	dir := '/opt/mcexamples'.
	(Smalltalk classNamed: #OSPlatform) ifNotNil: [ :c | 
		c current isWindows
			ifTrue: [ dir := 'c:\opt\mcexamples' ] ].
	^ dir