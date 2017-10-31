examples
examplesForegroundUpdate
	"self examplesForegroundUpdate"

	| transcript length |
	
	transcript := self new.
	length := 20.
	
	transcript 
		updateUsing: TranscriptZMorphicUpdateStrategy methodExampleBlock 
		while: [ 
			
			transcript 
				open;
				cr;
				show: 'STARTING----->'.	"Foreground updates"
					
			(Delay forMilliseconds: 60) wait .	
					
			1000 to: 1000 + length do: [ :i | 
				transcript show: '---' , i printString , '---'.
				(Delay forSeconds: 1) wait 
			]
		
		].
	