instance creation
createFromFile: aFileName ifFailed: aBlock
	"Right now, this protocol is Cairo backend only. "
	
 	| surface cstring |
	cstring := aFileName , (Character value: 0) asString.
	surface :=  self primCreateFromFile: cstring.
	surface isSuccess not
		ifTrue: [ 
			self destroySurfaceHandle: surface handle asInteger.
			^ aBlock cull: surface status ]
		ifFalse: [ surface initialize ].
	^ surface .