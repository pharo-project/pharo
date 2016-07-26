This Warning is signalled by methods which are deprecated.

The use of Object>>#deprecatedExplanation: aString and Object>>#deprecated: aBlock explanation: aString is recommended.

Idiom: Imagine I want to deprecate the message #foo.

foo
	^ 'foo'

I can replace it with:

foo
	self deprecatedExplanation: 'The method #foo was not good. Use Bar>>newFoo instead.'
	^ 'foo'

Or, for certain cases such as when #foo implements a primitive, #foo can be renamed to #fooDeprecated.

fooDeprecated
	^ <primitive>

foo
	^ self deprecated: [self fooDeprecated] explanation: 'The method #foo was not good. Use Bar>>newFoo instead.'
