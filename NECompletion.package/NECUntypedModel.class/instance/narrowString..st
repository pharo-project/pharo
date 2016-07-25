initialize-release
narrowString: aString 
	(narrowString isEmpty
			or: [aString isEmpty
			or: [aString first ~= narrowString first]])
		ifTrue: [self reset].
	super narrowString: aString