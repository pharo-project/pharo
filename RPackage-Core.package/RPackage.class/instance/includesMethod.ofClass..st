system compatibility
includesMethod: aSymbol ofClass: aClass

	self flag: #stef. "only used by RB"
	self flag: #cyrille. "looks like a bogus implementation because category have nothing to do with the fact that a method is in package or not."
	"CD: Indeed, it looks strange. 
	With RPackage we have 'includesSelector:ofClass:. I guess it should return the same result (?)'
	"
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass organization categoryOfElement: aSymbol)
										ifNil: [' '])
		ofClass: aClass