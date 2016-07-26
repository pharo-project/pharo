accessing
classNamesWithVariables
	| classNames |
	classNames := Set new.
	classNames
		addAll: instanceVariables keys;
		addAll: instanceVariableReaders keys;
		addAll: instanceVariableWriters keys;
		addAll: classVariables keys.
	^classNames