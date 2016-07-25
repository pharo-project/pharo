private
checkPackages: project
	| packages proj |
	
	packages := project packages.
	
	proj := packages at: 1.
	self assert: proj notNil.
	self assert:  proj name equals: 'VersionnerTestXML-Parser'.
	self assert: proj dependencies equals: (OrderedCollection newFrom: #('BitmapCharacterSet' 'XMLWriter')).
	
	proj := packages at: 2.
	self assert: proj notNil.
	self assert:  proj name equals: 'VersionnerTestXML-Tests-Parser'.
	self assert: proj dependencies equals: (OrderedCollection newFrom: #('VersionnerTestXML-Parser')).
