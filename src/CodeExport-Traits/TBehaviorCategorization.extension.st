Extension { #name : #TBehaviorCategorization }

{ #category : #'*CodeExport-Traits' }
TBehaviorCategorization classSide >> fileOutLocalMethodsInCategory: arg1 on: arg2 [

	| tmp1 |
	arg2 cr.
	tmp1 := self selectorsToFileOutCategory: arg1.
	tmp1 do: [ :arg3 |
		((self isLocalSelector: arg3) or: [
			 (self traitComposition selectors includes: arg3) not ]) ifTrue: [
			self printMethodChunk: arg3 on: arg2 ] ]
]
