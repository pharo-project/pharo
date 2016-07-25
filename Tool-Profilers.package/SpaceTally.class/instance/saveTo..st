fileout
saveTo: aFileName
	"| st |
	st := SpaceTally new.
	st spaceTally: (Array with: TextMorph with: Point).
	st saveTo: 'spaceTally2'"
	| file |
	file := aFileName asFileReference.
	file exists ifTrue: [ file delete ].
	
	file writeStreamDo: [ :s|
		results do: [:each | s nextPutAll: each analyzedClassName asString ; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; cr]].