testing
testRemoveFromSystem
	self flag: #fix.	"It has to be refactored when Mariano's ClassBuilder for testCase is integrated :)"
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ 
			| aClass aTrait |
			[ 
			aTrait := self createTrait.
			aClass := self createClassUsing: aTrait.
			aTrait removeFromSystem: false.
			self deny: aClass hasTraitComposition.
			self assert: aTrait isObsolete. ]
				ensure: [ aClass removeFromSystem: false ] ]