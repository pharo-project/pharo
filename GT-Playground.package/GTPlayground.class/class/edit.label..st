compatibility-tools
edit: aString label: aLabel

"Open a new playground with the given contents"
"Compatibility with the original Workspace"
"#edit: aText label: labelString accept: anAction"
"	^(Smalltalk tools workspace openLabel: labelString)
		acceptContents:  aText;
		acceptAction: anAction;
		yourself."

"Open an editor on the given string/text"
	| page |
	"self halt"

	page := GTPlayPage new
		saveContent: aString;
		title: aLabel;
		yourself.
		
	self flag: #TODO. "We need to put an equivalent to the acceptAction thing."
	^ self new openOn: page