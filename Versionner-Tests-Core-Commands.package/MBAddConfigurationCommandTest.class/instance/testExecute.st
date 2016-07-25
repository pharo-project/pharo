as yet unclassified
testExecute
	| t v |
	t := false.
	command := MBAddConfigurationCommand target: self for: nil.
	[[command execute] valueSupplyingAnswer: self configurationName  ] 
		on: Error do: [:ex | 
			t := ex messageText = 'Class #ConfigurationOfMetacello_Command_Tests already exist' ].
	self assert: t.
	
	v := 10000 atRandom.
	self should: [[command execute] valueSupplyingAnswers: 
		{{'Name of the new Metacello configuration (e.g., ConfigurationOfYourSoftware)'  . self configurationName, v asString  } .
		  {'Enter initial version number' . '1.0'} ".
		  {'Would you like to use http://www.squeaksource.com/Metacello_Command_Tests2, for your project?' . true}"
		}] raise: Exception.
		
	self deny: (Smalltalk globals includesKey: (self configurationName, v asString) asSymbol ).
