testing
testTraitRemoval
	self flag: #fix.	"It has to be refactored when Mariano's ClassBuilder for testCase is integrated :)"
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ 
			| aClass aTrait |
			[ 
			aTrait := self createTrait.
			aClass := self createClassUsing: aTrait.
			self assert: aClass localSelectors isEmpty.
			aClass removeFromComposition: aTrait.
			self assert: aClass localSelectors isEmpty.
			self assert: aClass organization allMethodSelectors isEmpty.
			self deny: aClass hasTraitComposition.
			self deny: (aTrait traitUsers includes: aClass). ]
				ensure: [ 
					aClass removeFromSystem: false.
					aTrait removeFromSystem: false ] ]