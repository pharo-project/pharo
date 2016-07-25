testing
testEnsureDefaultEnvironmentNotUsed
	
	| allClasses originalProtoObjectClass originalObjectClass anotherObjectClass |
	allClasses := self systemNavigationToTest allClasses.
	self assert: allClasses isEmpty.
	originalProtoObjectClass := Smalltalk globals at: #ProtoObject.		
	originalObjectClass := Smalltalk globals at: #Object.
	
	DangerousClassNotifier disableDuring: [
		anotherObjectClass := self classFactory newClassNamed: #Object subclassOf: originalProtoObjectClass instanceVariableNames: '' classVariableNames:  'DependentsFields'].
	
	self assert: ((self environmentToTest at: #Object) = anotherObjectClass).
	self assert: ((Smalltalk globals at: #Object) = originalObjectClass).
	