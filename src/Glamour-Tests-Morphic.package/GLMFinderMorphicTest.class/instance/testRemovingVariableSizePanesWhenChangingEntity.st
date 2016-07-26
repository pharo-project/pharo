tests
testRemovingVariableSizePanesWhenChangingEntity
	| finder |
	finder := GLMFinder new variableSizePanes.
	window := finder openOn: #a.
	self assert: window submorphs last scrollTarget submorphs size equals: 2.
	finder entity: #b.
	self assert: window submorphs last scrollTarget submorphs size equals: 2