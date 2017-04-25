private
wrapItem: anItem
	| s packageName |
	s := String
		streamContents: [ :aStream | 
			3 to: (cachedHierarchy at: anItem) size do: [ :i | aStream << '    ' ].
			aStream << (self methodClassNameForItem: anItem) << ' ('.
			anItem isFromTrait
				ifTrue: [ aStream
						<< anItem compiledMethod origin name;
						space ].
			aStream << (self protocolNameForItem: anItem) << ')' ].
	packageName := self packageNameForItem: anItem.
	^ {s. anItem selector. ('[' , packageName , ']')}