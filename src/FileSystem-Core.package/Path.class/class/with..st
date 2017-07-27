private
with: aString
	"Answer a relative path of the given string. N.B. that the argument is not parsed; 
	it is the name of a single path element, and path separators in it do not have 
	special meaning."
	
	| inst |
	(aString includes:$/ ) ifTrue: [ self error:' / shouldn''t be used in the String. The string should be the last element of a path only. Refer to Path>>with:'].
	
	inst := self new: 1.
	inst at: 1 put: aString.
	^ inst