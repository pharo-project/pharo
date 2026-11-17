Class {
	#name : 'SlotErrorsTest',
	#superclass : 'SlotSilentTest',
	#category : 'Kernel-CodeModel-Tests-ClassBuilder',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'ClassBuilder'
}

{ #category : 'tests' }
SlotErrorsTest >> testCannotBeRecompiled [
	| superclass |
	<expectedFailure>
	superclass := self make: [ :builder |
		builder
			superclass: Object;
			name: self aClassName ].

	self make: [ :builder |
		builder
			superclass: superclass;
			name: self anotherClassName ].

	self
		should: [
			self make: [ :builder |
				builder
					layoutClass: VariableLayout;
					name: self aClassName ] ]
		raise: Error
		description: 'Old class builder raises: X cannot be recompiled'
]

{ #category : 'tests' }
SlotErrorsTest >> testClassSlotDuplicationConflict [

	aClass := self make: [ :builder |
		builder classSlots: { #a } ].

	self should: [
		self make: [ :builder |
			builder
				superclass: aClass;
				name: self anotherClassName;
				classSlots: { #a } ] ]
		raise: DuplicatedSlotName
]

{ #category : 'tests' }
SlotErrorsTest >> testSlotDuplicationConflict [

	aClass := self make: [ :builder |
		builder slots: { #a } ].

	self should: [
		self make: [ :builder |
			builder
				superclass: aClass;
				name: self anotherClassName;
				slots: { #a } ] ]
		raise: DuplicatedSlotName
]
