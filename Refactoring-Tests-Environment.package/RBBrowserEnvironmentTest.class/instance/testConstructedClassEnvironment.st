testing
testConstructedClassEnvironment
	| environment |
	environment := RBClassEnvironment new.
	environment
		addClass: Object;
		addClass: OrderedCollection;
		addClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment includesClass: OrderedCollection).
	self assert: (environment includesClass: Collection).
	self deny: (environment includesClass: Object class).
	environment removeClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment includesClass: OrderedCollection).
	self deny: (environment includesClass: Collection).
	self assert: environment numberClasses = 2.
	environment addClass: Object class.
	self assert: environment numberClasses = 2.
	self assert: (environment includesClass: Object class).
	environment removeClass: self class.
	self assert: environment numberClasses = 2