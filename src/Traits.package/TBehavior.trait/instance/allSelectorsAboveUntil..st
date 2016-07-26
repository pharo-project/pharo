accessing method dictionary
allSelectorsAboveUntil: aRootClass

	| coll |
	coll := IdentitySet new.
	(self allSuperclassesIncluding: aRootClass) 
		do: [:aClass | 
				aClass selectorsDo: [ :sel | coll add: sel ]].
	^ coll
	

