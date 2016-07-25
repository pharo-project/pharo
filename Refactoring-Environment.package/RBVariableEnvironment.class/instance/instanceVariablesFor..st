accessing
instanceVariablesFor: aClass 
	| vars name |
	vars := Set new.
	name := aClass name.
	vars
		addAll: (instanceVariables at: name ifAbsent: [#()]);
		addAll: (instanceVariableReaders at: name ifAbsent: [#()]);
		addAll: (instanceVariableWriters at: name ifAbsent: [#()]).
	^vars