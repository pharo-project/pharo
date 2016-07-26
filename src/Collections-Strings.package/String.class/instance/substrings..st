converting
substrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream substringStream |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	substringStream := String new writeStream.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				substringStream isEmpty ifFalse: [
					result add: substringStream contents.
					substringStream := String new writeStream ] ]
			ifFalse: [
				substringStream nextPut: char ] ].
	substringStream isEmpty ifFalse: [
		result add: substringStream contents ].
	^ result asArray