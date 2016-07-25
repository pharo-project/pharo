tests - includes
testIdentityIncludesNonSpecificComportement
	" test the same comportement than 'includes: '  "
	| collection |	
	collection := self nonEmpty  .
	
	self deny: (collection identityIncludes: self elementNotIn ).
	self assert:(collection identityIncludes: collection anyOne)
