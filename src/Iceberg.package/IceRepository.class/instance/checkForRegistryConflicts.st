repository handling
checkForRegistryConflicts
	self class registry 
		detect: [ :repo | repo location = self location ]
		ifFound: [ :repo | self origin = repo origin
			ifTrue: [ 
				IceDuplicatedRepository signal: 
					('You already have an Iceberg repository 
at {1} 
and with the same origin URL. 

If you really whant create another one, 
please locate it in a different directory.'
					 format: {self location pathString}) ]
			ifFalse: [
				IceDuplicatedRepository signal: 
					('You already have an Iceberg repository at 
{1} 
but with a different origin URL.

You provided {2} 
and the original repository''s url is {3}.'
					 format: {self location. self origin. repo origin}) ]].