instance creation
from: aString delimiter: aDelimiterCharacter 
	"Answer a path composed of several elements delimited by aCharacter"
	| pathClass |
	aString isEmpty
		ifTrue: [ ^ self root ].
	
	pathClass :=  ((self isAbsolutePath: aString delimiter: aDelimiterCharacter) or: 
							[self isAbsoluteWindowsPath: aString]) 
		ifTrue: [ AbsolutePath ]
		ifFalse:[ RelativePath ].
	
	^ pathClass withAll: (aDelimiterCharacter split: aString)