instance creation
fromString36: aString
	"Decode a UUID from a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems."

	| object num |
	object := self nilUUID.
	num := Integer readFrom: aString asUppercase readStream base: 36.
	1 to: 16 do: [:i | object at: i put: (num digitAt: i)].
	^object