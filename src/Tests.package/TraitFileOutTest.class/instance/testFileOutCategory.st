testing
testFileOutCategory
	"File out whole system category, delete all classes and traits and then
	file them in again."

	"self run: #testFileOutCategory"

	| file |
	self class environment organization fileOutCategory: self categoryName.
	self class environment organization removeSystemCategory: self categoryName.
	self deny: (Smalltalk globals keys asSet includesAnyOf: #(#CA #CB #TA #TB #TC #TD)).
	[ 
	file := FileStream readOnlyFileNamed: self categoryName , '.st'.
	file fileIn ] ensure: [ file close ].
	self assertCollection: Smalltalk globals keys asSet includesAll: #(#CA #CB #TA #TB #TC #TD).
	ta := Smalltalk globals at: #TA.
	self assert: ta traitComposition asString equals: 'TB @ {#cc->#c} - {#c} + (TC @ {#cc->#c} - {#c})'.
	self assertCollection: ta selectors asSet includesAll: #(#a #b #cc).
	cb := Smalltalk globals at: #CB.
	self assert: cb traitComposition asString equals: 'TA'.
	self assertCollection: cb selectors asSet includesAll: #(#cb #a #b #cc).	"test classSide traitComposition of CB"
	self assert: cb classSide traitComposition asString equals: 'TA classTrait + TC'.
	self assertCollection: cb classSide selectors asSet includesAll: #(#d #c)