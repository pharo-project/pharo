as yet unclassified
genericRules 

	^ HelpTopic
		title: 'Generic Rules'
		contents: 
'To convert a generic rule (one that simply checks method, class or package) to work with Renraku you have to follow 3 simple steps:

Inheritance:
change superclass to ReAbstractRule.

Interest:
Specify which entities your rules is interested in by overriding a method on the class side to return true. If the rule was checking methods override #checksMethod, for classes override #checksClass, and for packages — #checksPackage.

Checking:
Implement #basicCheck: in a way that it will return true if the argument violated the rule and false otherwise. The quality tools will pass you only the arguments of the type you''ve expressed interest in.'