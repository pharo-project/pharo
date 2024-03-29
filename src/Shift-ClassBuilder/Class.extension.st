Extension { #name : 'Class' }

{ #category : '*Shift-ClassBuilder' }
Class >> addClassSlot: aSlot [
	<reflection: 'Class structural modification - Slot modification'>
	^ self classInstaller update: self to: [ :builder |
		builder
			classSlots: (self class classLayout slots copyWith: aSlot)]
]

{ #category : '*Shift-ClassBuilder' }
Class >> addSlot: aSlot [

	<reflection: 'Class structural modification - Slot modification'>
	^ self classInstaller
		  update: self
		  to: [ :builder | builder slots: (self localSlots copyWith: aSlot) ]
]

{ #category : '*Shift-ClassBuilder' }
Class >> removeClassSlot: aSlot [
	<reflection: 'Class structural modification - Slot modification'>
	^ self classInstaller update: self to: [ :builder |
		builder
			classSlots: (self class classLayout slots copyWithout: aSlot)]
]

{ #category : '*Shift-ClassBuilder' }
Class >> removeSlot: aSlot [
	<reflection: 'Class structural modification - Slot modification'>
	(self classLayout slots includes: aSlot) ifFalse: [
		self error: aSlot name , ' is not one of my slots' ].

	^ self classInstaller update: self to: [ :builder |
		  builder slots: (self classLayout slots copyWithout: aSlot) ]
]
