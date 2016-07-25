accessing
classVariablesFor: aClass 
	^classVariables at: aClass name ifAbsent: [#()]