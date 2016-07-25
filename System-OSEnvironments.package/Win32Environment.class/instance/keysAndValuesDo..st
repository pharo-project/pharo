enumeration
keysAndValuesDo: aBlock
	"Under windows the environemtn variables are a single big String."

	"Lines starting with an equal sign are invalid per    
	  http://stackoverflow.com/questions/10431689/what-are-these-strange-environment-variables"
	| environmentStrings nextString |

	environmentStrings := self environmentStrings.
	[ 
	nextString := environmentStrings fromCString.
	nextString ifEmpty: [ ^ self ].
	nextString first = $=
		ifFalse: [ self keysAndValuesDo: aBlock withAssociationString: nextString ].
	environmentStrings := environmentStrings + nextString size + 1 ] repeat