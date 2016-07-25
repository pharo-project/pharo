testing
testRemovingMethods
	| newMethod newClass |
	
	newClass:= RGClassDefinition named: #OrderedCollection.
	newMethod:= (RGMethodDefinition named: #add:)
					parent: newClass;
					protocol: 'adding';
					sourceCode: 'add: newObject
									^self addLast: newObject'.
	self assert: (newClass hasMethods not).

	newClass addMethod: newMethod.
	newClass addSelector: #size 
			   classified: 'accessing' 
			   sourced: 'size
							^ lastIndex - firstIndex + 1'.
	
	self assert: (newClass selectors asSet = #(add: size) asSet).
	newClass removeSelector: #join:.
	self assert: (newClass selectors asSet = #(add: size) asSet).
	newClass removeMethod: newMethod.
	self assert: ((newClass includesSelector: #add:) not).
	newClass removeSelector: #size.
	self assert: (newClass hasMethods not).