access
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue:
			[aFileStream nextPut: $!.
			aString :=  class asString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil:
				[aString := aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream cr].

	type == #preamble ifTrue: [aFileStream nextPut: $!].

	type == #classComment
		ifTrue:
			[aFileStream nextPut: $!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream cr].

	aFileStream nextChunkPut: self string.
	type == #method ifTrue: [aFileStream nextChunkPut: ' '].
	aFileStream cr