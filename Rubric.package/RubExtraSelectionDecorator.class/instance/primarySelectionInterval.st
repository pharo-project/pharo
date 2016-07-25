accessing
primarySelectionInterval
	
	self paragraph ifNotNil: [ :paragraph | 
		paragraph textArea ifNotNil: [ :textArea | 
			(textArea selectionStart isNotNil and: [ 
				textArea selectionStop isNotNil  		
			]) ifTrue: [ 
				^textArea selectionStart stringIndex to: textArea selectionStop stringIndex - 1
			]
		]	
	].

   ^0 to: -1
