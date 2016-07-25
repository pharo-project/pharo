printing
definitionString
	^String streamContents: [:aStream | aStream << '#' << self name << ' => '; << self class name].