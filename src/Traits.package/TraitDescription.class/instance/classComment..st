filein/out
classComment: aString
	"Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."
	^ self classComment: aString stamp: '<historical>'