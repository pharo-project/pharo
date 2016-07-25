initialize-release
initializeDefaultSequences
	"Until we have decent OSProcess support we precache the most common sequences"
	| esc |
	esc := (Character escape asString), '['.
	cache at: #up put: (esc, 'A').
	cache at: #le put: (esc, 'D').
	cache at: #nd put: (esc, 'C').
	cache at: #im put: ''.
	cache at: #ei put: ''.
	cache at: #mr put: (esc, '7m').
	cache at: #me put: (esc, '0m').
	cache at: #ce put: (esc, 'K').
	cache at: #al put: (esc, 'L').
	cache at: #dl put: (esc, 'M').
	cache at: #cr put: (esc, String cr).