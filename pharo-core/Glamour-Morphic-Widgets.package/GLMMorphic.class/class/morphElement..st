utils - morph creation
morphElement: anObject

	anObject isString
	ifTrue:[ ^StringMorph 
					contents: anObject 
					font: (LogicalFont 
						familyName: UITheme current buttonFont familyName
						pointSize: UITheme current buttonFont pointSize - 2) ].
	
	anObject isCollection 
	ifTrue: [ |row|
				row:= OrderedCollection new.
				anObject do: [ :e| row add: (self morphElement: e) ].
				^self alignmentMorph: row ].
				
	anObject isForm
	ifTrue:[ ^ImageMorph new form: anObject ].
	
	^self containerMorph