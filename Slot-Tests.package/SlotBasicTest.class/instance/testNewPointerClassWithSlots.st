tests-basic
testNewPointerClassWithSlots

	aClass := self 
		makeWithLayout: FixedLayout
		andSlots: { #id. #name }.
	
	self assert: (aClass instVarNames includes: 'id').
	self assert: (aClass instVarNames includes: 'name').