private
execute: aString replace: aRegex with: aReadStream
	| stream |
	^ aRegex copy: aString translatingMatchesUsing: [ :match |
		stream := WriteStream on: (String new: 2 * aString size).
		[ aReadStream atEnd ] whileFalse: [
			stream nextPutAll: (aReadStream upTo: $$).
			aReadStream atEnd ifFalse: [
				aReadStream peek isDigit
					ifFalse: [ stream nextPut: aReadStream next ]
					ifTrue: [ stream nextPutAll: (aRegex subexpression: aReadStream next asInteger - $0 asInteger + 1) ] ] ].
		aReadStream reset.
		stream contents ]