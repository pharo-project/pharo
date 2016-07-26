initialize-release
initializeAnalyzing: aClass

	super initializeAnalyzing: aClass.
	variablesMapping := FLVariablesMapping 
		newAnalyzing: theClass
		references: self references