*GT-SpotterExtensions-Core-private
withClassesFixedForNautilus
	classes ifEmpty: [ 
		metaClasses do: [ :each | 
			classes add: each ] ]