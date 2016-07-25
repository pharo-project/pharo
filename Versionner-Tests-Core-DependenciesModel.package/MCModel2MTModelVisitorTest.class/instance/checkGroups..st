private
checkGroups: project
	| groups group |
	
	groups := project groups sorted: [ :a :b | a name <= b name].
	
	self assert: groups size = 3.
	
	self assert: (groups collect: #name) asArray = #('Core' 'Tests' 'default').
	
	group := groups at: 1.
	self assert: group notNil.
	self assert:  group name equals: 'Core'.
	self assert: (group dependencies hasEqualElements: #('VersionnerTestXML-Parser')).
		
	group := groups at: 2.
	self assert: group notNil.
	self assert:  group name equals: 'Tests'.
	self assert: (group dependencies hasEqualElements: #('VersionnerTestXML-Tests-Parser')).
	
	group := groups at: 3.
	self assert: group notNil.
	self assert:  group name equals: 'default'.
	self assert: (group dependencies hasEqualElements: #('Core' 'Tests')).