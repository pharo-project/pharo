visiting
visitArgumentNodes: aNodeCollection 
	"Sent *once* when visiting method and block nodes"
	^aNodeCollection do: [ :each | self visitArgumentNode: each ]