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
	self assert: (Smalltalk globals keys asSet includesAll: #(#CA #CB #TA #TB #TC #TD)).
	ta := Smalltalk globals at: #TA.
	self assert: ta traitComposition asString = 'TB @ {#cc->#c} - {#c} + (TC @ {#cc->#c} - {#c})'.
	self assert: (ta selectors asSet includesAll: #(#a #b #cc)).
	cb := Smalltalk globals at: #CB.
	self assert: cb traitComposition asString = 'TA'.
	self assert: (cb selectors asSet includesAll: #(#cb #a #b #cc)).	"test classSide traitComposition of CB"
	self assert: cb classSide traitComposition asString = 'TA classTrait + TC'.
	self assert: (cb classSide selectors asSet includesAll: #(#d #c))