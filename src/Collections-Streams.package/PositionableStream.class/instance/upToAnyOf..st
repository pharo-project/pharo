accessing
upToAnyOf: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any object in the collection. If 
	no matching object is found, answer the entire rest of the receiver."
	^self upToAnyOf: aCollection do: [:matchingObject | ]