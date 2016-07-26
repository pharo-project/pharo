private - actions
newUpdatesFromString: aString
	"A string should be of following format:

	#Pharo2.0a
	20000-Pha-Empty.cs
	20001-Pha-Empty2.cs
	20002-Pha-DelScriptLoader.cs "
	
	| list out existing |
	
	out := OrderedCollection new.
	existing := SystemVersion current updates.
	list := self extractVersion: SystemVersion current version from: aString.
	
	list reverseDo: [:fileName | | itsNumber |
		itsNumber := fileName initialIntegerOrNil. 
		(existing includes: itsNumber)
			ifFalse: [ (itsNumber isNil or: [itsNumber <= self upToNumber])
							ifTrue: [out addFirst: self updateFolderURL, fileName]]
			ifTrue: [^ out]].
	out notEmpty  ifTrue:
				[^ out "we have our list"].	
	self feedback: 'All code update servers seem to be unavailable'.
	^ out
	
