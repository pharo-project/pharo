Extension { #name : #Class }

{ #category : #'*Shift-ClassInstaller' }
Class >> addClassSlot: aSlot [

 	<localClassMethod> 
   "This method is supposed to be local in Class because of a good reason.
   We use this pragma to test if Class does not contain some accidental 
   local selectors."

	^self classInstaller update: self to: [ :builder |
		builder
			superclass: self superclass;
			name: self name;
			layoutClass: self classLayout class;
			slots: self slots;
			classSlots: (self class classLayout slots copyWith: aSlot);
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment].
]

{ #category : #'*Shift-ClassInstaller' }
Class >> addSlot: aSlot [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^self classInstaller update: self to: [ :builder |
		builder
			superclass: self superclass;
			name: self getName;
			layoutClass: self classLayout class;
			slots: (self slots copyWith: aSlot);
			classSlots: self class classLayout slots;
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment].
]

{ #category : #'*Shift-ClassInstaller' }
Class >> removeSlot: aSlot [ 

	(self classLayout slots includes: aSlot)
		ifFalse: [self error: aSlot name , ' is not one of my slots'].
		
	^self classInstaller update: self to: [ :builder |
		builder
			superclass:  self superclass;
			name:  self name;
			layoutClass: self classLayout class;
			slots: (self classLayout slots copyWithout: aSlot);
			classSlots: self class classLayout slots;
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment]
	
]
