Class {
	#name : #MBLoadConfigurationCommandTest,
	#superclass : #TestCase,
	#category : 'Versionner-Tests-Core-Commands'
}

{ #category : #tests }
MBLoadConfigurationCommandTest >> testExecute [ 
	| block clsName |
	block := [(MBLoadConfigurationCommand target: nil) execute].
	clsName := 'ConfigurationOfHomeSweetHome'.
	
	Smalltalk globals at: clsName ifPresent: [ :cls | (Smalltalk globals at: clsName) removeFromSystem ].
	self should: 
		[block valueSupplyingAnswers: {
			{'Name of the new Metacello configuration (e.g., ConfigurationOfYourSoftware)'  . clsName} . 
			{'Would you like to use http://www.squeaksource.com/HomeSweetHome, for your project?' . true} .
			{'Enter initial version number' . '1.0'} }] raise: Exception

]
