instance creation
fromString: aString
	"Read a UUID from aString with my official representation, 32 lowercase hexadecimal (base 16) digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens)"

	| uuid |
	aString size ~= 36 ifTrue: [ self error: 'a UUID should be 36 characters' ].
	uuid := self nilUUID. 
	uuid readFrom: aString readStream.
	^ uuid