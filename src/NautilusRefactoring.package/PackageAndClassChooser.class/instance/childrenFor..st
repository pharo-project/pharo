accessing - tree
childrenFor: aPackageOrClass
	| result name |
	name := aPackageOrClass name.
	aPackageOrClass isBehavior 
		ifTrue: [ ^ #() ].
	
	aPackageOrClass isTrait 
		ifTrue: [ ^ #() ].
	result := Set new.
	RPackageOrganizer default packagesDo: [ :each | | tempName |
			tempName := each name.
			(tempName size > name size 
			and: [ (tempName at: name size + 1) = $- 
			and: [ (tempName beginsWith: name) ]])
				ifTrue: [ result add: each ]]. 
			
	result := result asSortedCollection: [ :a :b | a name <= b name ].
	result addAll: aPackageOrClass definedClasses.
	^ result
					