Extension { #name : 'VirtualMachine' }

{ #category : '*FileSystem-Disk' }
VirtualMachine >> binary [
	"Answer the a file reference to the virtual machine binary."
	"SmalltalkImage current vmBinary"

	^ FileLocator vmBinary
]

{ #category : '*FileSystem-Disk' }
VirtualMachine >> interpreterSourceDate [
	"Return the date of changes given by `self interpreterSourceVersion`"
	| dateString parts |

	dateString := (self interpreterSourceVersion splitOn: 'Date: ') second.

	dateString first isDigit
		ifTrue: [
			"most probably the date is in ISO 8601 Format"
			^ DateAndTime fromString: dateString ].

	"Otherwise assume the old format: DDD MMM DD HH:MM:SS YYYY +TTTT ..."
	parts := dateString substrings first: 6.
	"create a more reasonable string.."
	dateString := String streamContents: [ :s |
		s
			nextPutAll: (parts at: 5 ); space; "year"
			nextPutAll: (parts at: 2 ); space; "month name"
			nextPutAll: (parts at: 3 ); space; "day of month"
			nextPutAll: (parts at: 4 ); space; "time"
			nextPutAll: (parts last); space    "timezone"].
	^ dateString asDateAndTime
]
