*Network-UUID
asAlphaNumeric: totalSize extraChars: additionallyAllowed mergeUID: minimalSizeOfRandomPart 
	"Generates a String with unique identifier ( UID ) qualities, the difference to a
	 UUID is that its beginning is derived from the receiver, so that it has a meaning
	 for a human reader.

	 Answers a String of totalSize, which consists of 3 parts
	 1.part: the beginning of the receiver only consisting of
		a-z, A-Z, 0-9 and extraChars in Collection additionallyAllowed ( which can be nil )
	 2.part: a single _
	 3.part: a ( random ) UID of size >= minimalSizeOfRandomPart consisting of
		a-z, A-Z, 0-9

	 Starting letters are capitalized. 
	 TotalSize must be at least 1.
	 Exactly 1 occurrence of $_ is guaranteed ( unless additionallyAllowed includes $_ ).
	 The random part has even for small sizes good UID qualitites for many practical purposes.
	 If only lower- or uppercase letters are demanded, simply convert the answer with
	 say #asLowercase. The probability of a duplicate will rise only moderately ( see below ).

	 Example: 
		size of random part = 10
		in n generated UIDs the chance p of having non-unique UIDs is
			n = 10000 ->  p < 1e-10		if answer is reduced to lowerCase: p < 1.4 e-8
			n = 100000 -> p < 1e-8
		at the bottom is a snippet for your own calculations  
		Note: the calculated propabilites are theoretical,
			for the actually used random generator they may be much worse"
	| stream out sizeOfFirstPart index ascii ch skip array random |
	totalSize > minimalSizeOfRandomPart ifFalse: [ self errorOutOfBounds ].
	stream := self readStream.
	out :=  (String new: totalSize) writeStream.
	index := 0.
	skip := true.
	sizeOfFirstPart := totalSize - minimalSizeOfRandomPart - 1.
	[ stream atEnd or: [ index >= sizeOfFirstPart ] ] whileFalse: 
		[ (((ascii := (ch := stream next) asciiValue) between: 65 and: 90) or: 
			[ (ascii between: 97 and: 122) or: 
				[ ch isDigit or: [ additionallyAllowed notNil and: [ additionallyAllowed includes: ch ] ] ] ]) 
			ifTrue: 
				[ skip 
					ifTrue: [ out nextPut: ch asUppercase ]
					ifFalse: [ out nextPut: ch ].
				index := index + 1.
				skip := false ]
			ifFalse: [ skip := true ] ].
	out nextPut: $_.
	array := Array new: 62.
	1 
		to: 26
		do: 
			[ :i | 
			array 
				at: i
				put: (i + 64) asCharacter.
			array 
				at: i + 26
				put: (i + 96) asCharacter ].
	53 
		to: 62
		do: 
			[ :i | 
			array 
				at: i
				put: (i - 5) asCharacter ].
	random := UUIDGenerator default randomGenerator.
	totalSize - index - 1 timesRepeat: [ out nextPut: (array atRandom: random) ].
	^ out contents

	"	calculation of probability p for failure of uniqueness in n UIDs
		Note: if answer will be converted to upper or lower case replace 62 with 36
	| n i p all |
	all := 62 raisedTo: sizeOfRandomPart.
	i := 1.
	p := 0.0 .
	n := 10000.
	[ i <= n ]
	whileTrue: [
		p := p + (( i - 1 ) / all ).
		i := i + 1 ].
	p   

	approximation formula: n squared / ( 62.0 raisedTo: sizeOfRandomPart ) / 2 
	"

	"'Crop SketchMorphs and Grab Screen Rect to JPG' 
			asAlphaNumeric: 31 extraChars: nil mergeUID: 10  
	 			'CropSketchMorphsAndG_iOw94jquN6'
	 'Monticello' 
			asAlphaNumeric: 31 extraChars: nil mergeUID: 10    
				'Monticello_kp6aV2l0IZK9uBULGOeG' 
	 'version-', ( '1.1.2' replaceAll: $. with: $- )
			asAlphaNumeric: 31 extraChars: #( $- ) mergeUID: 10    
				'Version-1-1-2_kuz2tMg2xX9iRLDVR'"