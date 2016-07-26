instance creation
* aString
	"Answer a relative path with aString as its sole segment. For example,
	Path * 'griffle' will produce the same result as ./griffle in a unix shell.
	The selector #* was chosen for it's visual similarity to $."
	
	"Note: aString is not parsed, so supplying a string like '/griffle/plonk'
	will not create an absolute path."
	
	^ RelativePath with: aString