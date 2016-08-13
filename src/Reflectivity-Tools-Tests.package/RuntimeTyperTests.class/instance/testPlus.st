tests
testPlus
	| link variable |
	
	link := MetaLink new
		metaObject: #node;
		selector: #tagType:;
		arguments: #(value);
		control: #after.
		
	(self class>>#examplePlus:) ast assignmentNodes do: [ :node | node link: link ].
	
	self assert: (self examplePlus: 1) = 2.
	self assert: (self examplePlus: 1.1) = 2.1.
	
	"link uninstall."
	
	variable := (self class>>#examplePlus:) ast assignmentNodes first variable.
	self assert: ((variable propertyAt: #types) includes: SmallInteger).  
		

		