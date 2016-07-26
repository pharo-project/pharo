I am a refactoring for inlining code in all senders.

For example, a method 
foo
	^ 'text'.
	
is called in
baz
	|a|
	a:= self foo.
	^ self foo.
	
inlining in all senders replaces the call to method foo, with its code:

baz
	|a|
	a:= 'text'.
	^ 'text'.

The method itself is not removed!