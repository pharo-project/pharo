private
wrapItem: anItem
	| s packageName |
	
	s :=String streamContents: [ :aStream | 
		3 to: (cachedHierarchy at: anItem) size do: [:i | aStream << '    '].
		aStream 
			<< anItem methodClass name << ' ('.
		anItem isFromTrait
			ifTrue: [ aStream << anItem compiledMethod origin name; space ].
		aStream << (anItem category ifNil: ['']) <<')'].
   packageName := anItem package ifNil:[ '' ] ifNotNil:[:package| package name].
	^ {s. anItem selector.  '[', packageName,']'.}.