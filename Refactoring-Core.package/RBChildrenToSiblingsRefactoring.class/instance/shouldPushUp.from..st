private-methods
shouldPushUp: aSelector from: aClass 
	^ ((aClass isMeta 
		ifTrue: [ subclasses collect: [ :each | each theMetaClass ] ]
		ifFalse: [ subclasses ]) 
		detect: [ :each | (each directlyDefinesMethod: aSelector) not ]
		ifNone: [ nil ]) notNil