Class {
	#name : #MetaLinkObjectAPITests,
	#superclass : #TestCase,
	#instVars : [
		'tag'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #initialization }
MetaLinkObjectAPITests >> link [
	^ MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(object);
		control: #after
]

{ #category : #initialization }
MetaLinkObjectAPITests >> linkForTemp [
	| okNode |
	okNode := RBLiteralValueNode value: 'ok'.
	^ MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: (Array with: okNode);
		control: #after
]

{ #category : #running }
MetaLinkObjectAPITests >> setUp [
	super setUp.
	tag := OrderedCollection new.
	MetaLink uninstallAll 
]

{ #category : #tagging }
MetaLinkObjectAPITests >> tagExec: aTag [
	tag add: aTag
]

{ #category : #running }
MetaLinkObjectAPITests >> tearDown [
	MetaLink uninstallAll.
	super tearDown.
]

{ #category : #'class api' }
MetaLinkObjectAPITests >> testLinkClassToAST [
	|link instance|	
	link := self link.
		
	ReflectivityExamples2 link: link toAST: (ReflectivityExamples2 >> #exampleMethod) ast.
	self assert: tag isEmpty.
	instance := ReflectivityExamples2 new.
	instance exampleMethod.
	self assert: tag size = 1.
	self assert: tag first == instance
	
		
]

{ #category : #'class api' }
MetaLinkObjectAPITests >> testLinkClassToClassVarNamed [
	|link instance|	
	link := self link.
		
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar.
	self assert: tag isEmpty.
	
	instance := ReflectivityExamples2 new.
	instance methodWithClassVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
	
]

{ #category : #'class api' }
MetaLinkObjectAPITests >> testLinkClassToSlotNamed [

	|link instance|	
	link := self link.
		
	ReflectivityExamples2 link: link toSlotNamed: #instVar.
	self assert: tag isEmpty.
	
	instance := ReflectivityExamples2 new.
	instance methodWithInstVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'class api' }
MetaLinkObjectAPITests >> testLinkClassToTempVarNamed [
	|link instance|	
	link := self linkForTemp.
		
	ReflectivityExamples2 link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess.
	self assert: tag isEmpty.
	
	instance := ReflectivityExamples2 new.
	instance methodWithTempVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t = 'ok'])
]

{ #category : #'object - api' }
MetaLinkObjectAPITests >> testLinkObjectToAST [
	|link instance instance2|	
	link := self link.	

	instance := ReflectivityExamples2 new.
	instance2 := ReflectivityExamples2 new.
	
	instance link: link toAST: (ReflectivityExamples2 >> #exampleMethod) ast.
	self assert: tag isEmpty.
	
	instance exampleMethod.
	instance2 exampleMethod.
	self assert: tag size = 1.
	self assert: tag first == instance
]

{ #category : #'object - api' }
MetaLinkObjectAPITests >> testLinkObjectToClassVarName [
	|link instance |	
	self skip.
	self flag: 'must be fixed'.
	
	link := self link.
	
	instance := ReflectivityExamples2 new.	
	
	instance link: link toClassVariableNamed: #classVar.
	self assert: tag isEmpty.	

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'object - api' }
MetaLinkObjectAPITests >> testLinkObjectToSlotNamed [

	|link instance |	
	link := self link.
	
	instance := ReflectivityExamples2 new.	
	
	instance link: link toSlotNamed: #instVar.
	self assert: tag isEmpty.	

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'object - api' }
MetaLinkObjectAPITests >> testLinkObjectToTempVarName [

	|link instance |	
	link := self link.
	
	instance := ReflectivityExamples2 new.	
	
	instance link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess.
	self assert: tag isEmpty.	

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToClassVarNameAll [

	|link instance |	
	link := self link.
	
	"For all instances"
	instance := ReflectivityExamples2 new.		
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar option: #all.
	self assert: tag isEmpty.	

	instance methodWithClassVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"	
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toClassVariableNamed: #classVar option: #all.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToClassVarNameReads [

	|link instance |	
	link := self link.
	
	"For all instances"
	instance := ReflectivityExamples2 new.		
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar option: #read.
	self assert: tag isEmpty.	

	instance methodWithClassVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"	
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toClassVariableNamed: #classVar option: #read.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToClassVarNameWrites [
	|link instance |	
	link := self link.
	
	"For all instances"
	instance := ReflectivityExamples2 new.		
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar option: #write.
	self assert: tag isEmpty.	

	instance methodWithClassVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"	
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toClassVariableNamed: #classVar option: #write.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToSlotNamedAll [

	|link instance |	
	link := self link.	
	instance := ReflectivityExamples2 new.	

	"For all instances"	
	ReflectivityExamples2 link: link toSlotNamed: #instVar option: #all.
	self assert: tag isEmpty.	

	instance methodWithInstVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toSlotNamed: #instVar option: #all.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToSlotNamedReads [

	|link instance |	
	link := self link.	
	instance := ReflectivityExamples2 new.	

	"For all instances"	
	ReflectivityExamples2 link: link toSlotNamed: #instVar option: #read.
	self assert: tag isEmpty.	

	instance methodWithInstVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toSlotNamed: #instVar option: #read.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToSlotNamedWrites [
	|link instance |	
	link := self link.	
	instance := ReflectivityExamples2 new.	

	"For all instances"	
	ReflectivityExamples2 link: link toSlotNamed: #instVar option: #write.
	self assert: tag isEmpty.	

	instance methodWithInstVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toSlotNamed: #instVar option: #write.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToTempVarNameAll [

	|link instance |	
	link := self link.	
	instance := ReflectivityExamples2 new.	
	
	"For all instances"
	ReflectivityExamples2 link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess option: #all.
	self assert: tag isEmpty.	

	instance methodWithTempVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess option: #all.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.
	
	self assert: tag size = 4.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToTempVarNameReads [
	|link instance |	
	link := self link.	
	instance := ReflectivityExamples2 new.	
	
	"For all instances"
	ReflectivityExamples2 link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess option: #read.
	self assert: tag isEmpty.	

	instance methodWithTempVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess option: #read.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]

{ #category : #'common api' }
MetaLinkObjectAPITests >> testLinkToTempVarNameWrites [
	|link instance |	
	link := self link.	
	instance := ReflectivityExamples2 new.	
	
	"For all instances"
	ReflectivityExamples2 link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess option: #write.
	self assert: tag isEmpty.	

	instance methodWithTempVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance]).
	
	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess option: #write.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.
	
	self assert: tag size = 2.
	self assert: (tag allSatisfy: [ :t| t == instance])
]
