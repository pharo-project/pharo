copying
postCopy
	| oldFlag |
	super postCopy.
	oldFlag := flag.
	flag := Object new.
	array replaceAll: oldFlag with: flag.