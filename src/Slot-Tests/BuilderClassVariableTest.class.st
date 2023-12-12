Class {
	#name : 'BuilderClassVariableTest',
	#superclass : 'SlotClassBuilderTest',
	#category : 'Slot-Tests-ClassBuilder',
	#package : 'Slot-Tests',
	#tag : 'ClassBuilder'
}

{ #category : 'tests' }
BuilderClassVariableTest >> testClassVariableDoesNotDuplicatesSubclassesOfSuperclass [
	"Issue: 13028"
	| class1 class2 |

	class1 := self make: [ :builder |
		builder
			name: self aClassName;
			superclass: Object ].

	class2 := self make: [ :builder |
		builder
			name: self anotherClassName;
			superclass: class1 ].

	self assert: class1 subclasses equals: { class2 }.
	class2 := self make: [ :builder |
		builder
			name: self anotherClassName;
			superclass: class1;
			sharedVariables: #(ASharedVariable) ].
	self assert: class1 subclasses equals: { class2 }
]
