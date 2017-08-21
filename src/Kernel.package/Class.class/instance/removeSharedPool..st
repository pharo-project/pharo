pool variables
removeSharedPool: aDictionary 
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool 
	dictionaries. Create an error notification if the dictionary is not one of 
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."

	| satisfiedSet workingSet aSubclass |
	(self sharedPools includes: aDictionary)
		ifFalse: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [:class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [self sharedPools remove: aDictionary.
				self sharedPools isEmpty ifTrue: [self sharedPools: nil].
				^self]. 

	"second get all the subclasses that reference aDictionary through me rather than a 
	superclass that is one of my subclasses."

	workingSet := self subclasses asOrderedCollection.
	satisfiedSet := Set new.
	[workingSet isEmpty] whileFalse:
		[aSubclass := workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse: 
				[satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because 
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do: 
		[:sub | 
		aDictionary associationsDo: 
			[:aGlobal | 
			(sub whichSelectorsReferTo: aGlobal) isEmpty 
				ifFalse: [^self error: aGlobal key 
								, ' is still used in code of class '
								, sub name]]].
	self sharedPools remove: aDictionary.
	self sharedPools isEmpty ifTrue: [self sharedPools: nil]