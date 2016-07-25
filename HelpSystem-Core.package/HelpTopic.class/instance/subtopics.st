accessing
subtopics 
	"Returns the receivers list of subtopics"
	
	^ subtopics ifNil: [subtopics := OrderedCollection new]