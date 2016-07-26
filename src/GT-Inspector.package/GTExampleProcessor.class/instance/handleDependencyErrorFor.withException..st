private
handleDependencyErrorFor: anExample withException: anException
	^ (GTExampleDependencyError new
		example: anExample;
		exception: anException;
		yourself) signal