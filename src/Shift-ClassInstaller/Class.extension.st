Extension { #name : #Class }

{ #category : #'*Shift-ClassInstaller' }
Class >> addClassSlot: aSlot [

	^ self classInstaller update: self to: [ :builder |
		builder
			superclass: self superclass;
			name: self name;
			layoutClass: self classLayout class;
			slots: self slots;
			classSlots: (self class classLayout slots copyWith: aSlot);
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment;
			copyClassSlotsFromExistingClass ]
]

{ #category : #'*Shift-ClassInstaller' }
Class >> addSlot: aSlot [

	^self classInstaller update: self to: [ :builder |
		builder
			fillFor: self;
			slots: (self localSlots copyWith: aSlot)].
]

{ #category : #'*Shift-ClassInstaller' }
Class >> removeSlot: aSlot [ 
	
	(self classLayout slots includes: aSlot)
		ifFalse: [self error: aSlot name , ' is not one of my slots'].
		
	^self classInstaller update: self to: [ :builder |
		builder
			fillFor: self;
			slots: (self classLayout slots copyWithout: aSlot)]
	
]
