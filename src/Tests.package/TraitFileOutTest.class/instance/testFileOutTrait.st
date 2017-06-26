testing
testFileOutTrait
	"fileOut trait T6, remove it from system and then file it in again"

	"self run: #testFileOutTrait"

	| fileName file |
	self t6 compile: 'localMethod: argument ^argument'.
	self t6 classSide compile: 'localClassSideMethod: argument ^argument'.
	self t6 fileOut.
	fileName := self t6 asString , '.st'.
	self resourceClassesAndTraits remove: self t6.
	self t6 removeFromSystem.
	[ 
	file := FileStream readOnlyFileNamed: fileName.
	file fileIn ] ensure: [ file close ].
	self assert: (Smalltalk globals includesKey: #T6).
	TraitsResource current t6: (Smalltalk globals at: #T6).
	self resourceClassesAndTraits add: self t6.
	self assert: self t6 traitComposition asString = 'T1 + (T2 @ {#m22Alias->#m22})'.
	self assert: (self t6 selectors asSet includesAll: #(#localMethod: #m11 #m12 #m13 #m21 #m22 #m22Alias)).
	self assert: self t6 classSide methods size = 2.
	self assert: (self t6 classSide selectors asSet includesAll: #(#localClassSideMethod: #m2ClassSide:))