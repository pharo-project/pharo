I am a refactoring for inlining code of this method.

The call to this method in all other methods of this class is replaced by its implementation. The method itself will be removed.

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

