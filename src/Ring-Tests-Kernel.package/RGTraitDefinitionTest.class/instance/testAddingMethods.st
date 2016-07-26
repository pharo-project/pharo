testing
testAddingMethods
	| newMethod newClass |
	
	newClass:= RGTraitDefinition named:  #TSortable.
	newMethod:= (RGMethodDefinition named: #sort) parent: newClass;
					protocol: 'sorting'; 
					sourceCode: 'sort
									self sort: [:a :b | a <= b]'.
	
	self assert: (newMethod isMetaSide not).
	self assert: (newClass hasMethods not).

	newClass addMethod: newMethod.
	newClass addSelector: #size 
			   classified: 'accessing' 
			   sourced: 'foo
							^lastIndex - firstIndex + 1'.
	
	self assert: (newClass hasMethods).
	self assert: (newClass selectors = #(sort size)).
	self assert: (newClass includesSelector: #sort).
	self assert: ((newClass methodNamed: #sort) = newMethod).
	self assert: (newClass methods size = 2).
	self assert: (newClass selectors size = 2).
	self assert: (newClass allSelectors size = 2).  "no hierarchy"

	newMethod:= newClass methodNamed: #size.
	self assert: (newMethod parent = newClass).
	
	self assert: ((newClass compiledMethodNamed: #sort) notNil).
	self assert: ((newClass compiledMethodNamed: #foo) isNil)