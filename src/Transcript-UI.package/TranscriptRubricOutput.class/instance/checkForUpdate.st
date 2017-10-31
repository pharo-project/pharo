stepping
checkForUpdate

	cleared 
		ifTrue: [ textMorph setTextWith: stream contents  ]
		ifFalse: [
			stream contents ifNotEmpty: [ :content |
				"| txt |			
				txt := content asText.
				txt makeAllColor: Color random."
				textMorph textArea handleEdit: [ 
					textMorph appendText: content "txt" 
				 ].
				stream resetContents.
			]
		 ].
	
	cleared := false