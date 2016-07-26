package filter
class: aClass matchPackagePatternString: aString 
	| pattern |
	pattern := self createPackagePattern: aString.
	^ self package: aClass package matchPackagePattern: pattern