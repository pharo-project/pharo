stream in
nextLineLf
	| nextLine |
	nextLine := self upToAll: String lf.
	^nextLine