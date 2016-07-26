tests
testTraitUsersAfterClassReshape

	aClass := self make: [ :builder | 
		builder
			name: self aClassName ].

	anotherClass := self make: [ :builder | 
		builder
			name: self anotherClassName;
			superclass: aClass;
			traitComposition: TOne;
			classTraitComposition: TOne classTrait + TTwo ].
	
	self assert: TOne traitUsers asArray equals: { anotherClass }.
	self assert: TOne classTrait traitUsers asArray equals: { anotherClass class }.
	self assert: TTwo traitUsers asArray equals: { anotherClass class }.
	self assert: TTwo classTrait traitUsers asArray equals: { }.
	
	self deny: (aClass canUnderstand: #one).
	self assert: (anotherClass canUnderstand: #one).
	self deny: (aClass class canUnderstand: #two).
	self assert: (anotherClass class canUnderstand: #two).
	
	
	"reshape the super class"
	aClass := self make: [ :builder | 
		builder
			name: self aClassName;
			slots: #( x y z ) ].
		
	self assert: TOne traitUsers asArray equals: { anotherClass }.
	self assert: TOne classTrait traitUsers asArray equals: { anotherClass class }.
	self assert: TTwo traitUsers asArray equals: { anotherClass class }.
	self assert: TTwo classTrait traitUsers asArray equals: { }.
	
	self deny: (aClass canUnderstand: #one).
	self assert: (anotherClass canUnderstand: #one).
	self deny: (aClass class canUnderstand: #two).
	self assert: (anotherClass class canUnderstand: #two).

