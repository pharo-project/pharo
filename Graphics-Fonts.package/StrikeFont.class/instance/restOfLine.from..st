file in/out
restOfLine: leadString from: file 
	"Utility method to assist reading of BitFont data files"
	| line |
	
	[ line := file nextLine.
	line size < leadString size or: [ leadString ~= (line 
			copyFrom: 1
			to: leadString size) ] ] whileTrue: [ file atEnd ifTrue: [ ^ nil ] ].
	^ line 
		copyFrom: leadString size + 1
		to: line size