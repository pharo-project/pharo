Extension { #name : #Number }

{ #category : #'*NumberParser' }
Number class >> readFrom: stringOrStream [ 
	"Answer a number as described on aStream.  The number may
	be any accepted Smalltalk literal Number format.
	It can include a leading radix specification, as in 16rFADE.
	It can as well be NaN, Infinity or -Infinity for conveniency.
	If stringOrStream does not start with a valid number description, fail."
	
	^(NumberParser on: stringOrStream) nextNumber
]

{ #category : #'*NumberParser' }
Number class >> readFrom: stringOrStream base: base [
	"Answer a number as described on aStream in the given number base.
	If stringOrStream does not start with a valid number description, answer 0 for backward compatibility. This is not clever and should better be changed."

	^(NumberParser on: stringOrStream) nextNumberBase: base
]

{ #category : #'*NumberParser' }
Number class >> readFrom: stringOrStream ifFail: aBlock [
	"Answer a number as described on aStream.  The number may
	be any accepted Smalltalk literal Number format.
	It can include a leading radix specification, as in 16rFADE.
	It can as well be NaN, Infinity or -Infinity for conveniency.
	If input does not represent a valid number, then execute fail block
	and leave the stream positioned before offending character"
	
	^(NumberParser on: stringOrStream) failBlock: aBlock; nextNumber
]

{ #category : #'*NumberParser' }
Number class >> squeezeNumberOutOfString: stringOrStream [
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ NumberParser squeezeNumberOutOfString: stringOrStream
]

{ #category : #'*NumberParser' }
Number class >> squeezeNumberOutOfString: stringOrStream ifFail: aBlock [
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ NumberParser squeezeNumberOutOfString: stringOrStream onError: aBlock
]
