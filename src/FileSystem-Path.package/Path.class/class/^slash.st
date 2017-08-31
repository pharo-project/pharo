instance creation
/ aString
	"Answer an absolute path with aString as it's sole segment. The selector
	was chosen to allow path construction with Smalltalk syntax, which 
	neverthelesss resembles paths as they appear in a unix shell. Eg.
	Path / 'griffle' / 'plonk'."
	
	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].
	^ AbsolutePath with: aString