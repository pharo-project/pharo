testing
testObsoleteClasses
	| obsoleteClasses |
	
	Smalltalk fixObsoleteReferences.
	Smalltalk garbageCollect.
	obsoleteClasses := SystemNavigation new obsoleteClasses
		select: [ :each | each isAnonymous not ].
	
	self 
		assert: obsoleteClasses isEmpty
		description: [
			String streamContents: [ :s|
				s 
					nextPutAll: 'Obsolete classes remaining: ';
					print: obsoleteClasses ]]