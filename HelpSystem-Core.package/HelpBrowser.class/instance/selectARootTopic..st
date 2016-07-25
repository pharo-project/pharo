actions
selectARootTopic: aHelpClass
	"Used to unfold a particular topic"
	
	| item |
	item := treeMorph allItems detect: [:each | 
            each complexContents item key = aHelpClass name ].
   treeMorph selection: item