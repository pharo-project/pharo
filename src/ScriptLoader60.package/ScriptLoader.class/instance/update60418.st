pharo - updates
update60418
	"self new update60418"
	self withUpdateLog: '19778 NaturalLanguageTranslator contains a class variable AllKnownPhrases in bootstrapped image
	https://pharo.fogbugz.com/f/cases/19778

19773 BaselineOfMorphic compiles own version of Character>>#characterSet
	https://pharo.fogbugz.com/f/cases/19773

19667 After removing of a package by Nautilus the empty extension protocols are still there
	https://pharo.fogbugz.com/f/cases/19667

18252 debugit on "self" causes endless loop
	https://pharo.fogbugz.com/f/cases/18252'.
	self loadTogether: self script60418 merge: false.
	self flushCaches.
