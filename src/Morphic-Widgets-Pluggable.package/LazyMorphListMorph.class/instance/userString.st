as yet unclassified
userString
	"Do I have a text string to be searched on?"

	^ String
		streamContents: [ :stream | 
			1 to: self getListSize do: [ :i | 
				(self getListItem: i) submorphs detect: [ :morph | morph userString notNil ] ifFound: [ :morph | stream nextPutAll: morph userString ].
				stream cr ] ]