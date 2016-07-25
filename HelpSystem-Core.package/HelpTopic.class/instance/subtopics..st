accessing
subtopics: aCollection 
	"Sets the receivers subtopics"
	
	aCollection do: [ :topic|  topic owner: self  ].
	subtopics := aCollection.