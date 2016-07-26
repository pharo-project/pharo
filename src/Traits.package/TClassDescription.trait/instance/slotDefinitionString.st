printing
slotDefinitionString
	"Answer a string that contains an executable description of my Slots"

	"^self slots ifNotEmpty: [self slots asString] ifEmpty: ['{}']"
	
	^String streamContents: [ :str | | special |
		str nextPutAll: '{ '.
		self slots do: [:slot |
				str nextPutAll: slot definitionString.
				special := slot isSpecial]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				special ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]
