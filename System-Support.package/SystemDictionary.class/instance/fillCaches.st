class and trait names
fillCaches
	"Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values."

	| classNames nonClassNames |
	classNames := OrderedCollection new: self size.
	nonClassNames := OrderedCollection new.
	self keysAndValuesDo: [ :key :value |
		"The key == value name test below addresses two separate issues:
			1) Obsolete classes, where key = #Foo and value name = 'AnObsoleteFoo'
			2) Aliases, i.e., Smalltalk at: #OtherName put: aClass"
		((value isKindOf: (self class environment at: #Class)) and: [ key == value name ])
			ifTrue: [ classNames add: key ]
			ifFalse: [ nonClassNames add: key ] ].
		"The expression (self class environment at: #Class) deserves some explanation.
		For bootstrapping we need to have a different Class when executing value isKindOf: Class :
		In such expression Class represents a kind of fixed point: the class of the class of the system. 
		When bootstrapping we want the class Class of the current kernel defined in the current namespace.
		Since the current namespace should contains the class that describes itself as well as a new Class class.
		we are done :)."
		
	classNames sort.
	cachedNonClassNames := nonClassNames sort.
	cachedClassNames := classNames.
	^{ classNames. nonClassNames }