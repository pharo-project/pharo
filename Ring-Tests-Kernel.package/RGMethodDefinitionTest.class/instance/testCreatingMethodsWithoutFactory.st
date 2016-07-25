testing
testCreatingMethodsWithoutFactory
	| newMethod newClass |

	newMethod := RGMethodDefinition realClass: OrderedCollection selector: 'size'.
	self assert: (newMethod isMethod).  
	self assert: (newMethod selector == #size).  
	self assert: (newMethod protocol notNil).
	self assert: (newMethod sourceCode notEmpty).
	self assert: (newMethod stamp notNil).
	
	newClass := RGClassDefinition named: #OrderedCollection.
	newMethod := RGMethodDefinition class: newClass selector: 'size'.
	self assert: (newMethod isMethod).  
	self assert: (newMethod selector == #size).  
	self assert: (newMethod protocol isNil).
	self assert: (newMethod sourceCode isNil).
	self assert: (newMethod stamp isNil).
	
	newMethod := RGMethodDefinition realClass: OrderedCollection theMetaClass selector: #arrayType.
	self assert: (newMethod isMethod).  
	self assert: (newMethod selector == #arrayType).  
	self assert: (newMethod protocol notNil).
	self assert: (newMethod sourceCode notEmpty).
	self assert: (newMethod stamp notNil).