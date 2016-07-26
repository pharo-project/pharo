tests
testClassObsolete
	| aClass obj |
	Smalltalk globals at: #ClassForObsoleteTest ifPresent: [ :cls | cls removeFromSystem ].
	aClass := classFactory newClass.
	obj := aClass new.
	self deny: aClass isObsolete.
	self deny: aClass class isObsolete.
	aClass removeFromSystem.
	self assert: aClass isObsolete.
	self assert: aClass class isObsolete