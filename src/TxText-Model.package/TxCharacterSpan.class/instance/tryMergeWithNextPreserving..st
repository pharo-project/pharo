operation
tryMergeWithNextPreserving: selection
	"Applicable only to character spans"
	
	"Merge with next span, if it has same attributes"
	characters size isZero ifTrue: [ ^ self delete ].
	
	[ next isCharacterSpan ] whileTrue: [
	
		next style = self style ifTrue: [
			characters := characters , next characters.
			
			"preserve the selection, while merging"
			selection start span == next ifTrue: [ 
				selection start: (self positionAt: (self size + selection start spanPosition))
				 ].
			
			selection end span == next ifTrue: [ 
				selection end: (self positionAt: (self size + selection end spanPosition))
				 ].

			next delete.
		] ifFalse: [ ^ self ]
	]
