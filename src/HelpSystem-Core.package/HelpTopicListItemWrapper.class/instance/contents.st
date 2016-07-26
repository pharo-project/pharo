accessing
contents
	"Returns subnodes (if any)"
	
 	item hasSubtopics ifFalse: [^#()].	
	^(item subtopics) collect: [ :each | 
		HelpTopicListItemWrapper with: each
	].
