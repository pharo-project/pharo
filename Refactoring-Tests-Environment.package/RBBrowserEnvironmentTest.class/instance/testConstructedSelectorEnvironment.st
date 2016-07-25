testing
testConstructedSelectorEnvironment
	| environment newEnvironment |
	environment := RBSelectorEnvironment new.
	environment
		addClass: Object selector: #printString;
		addClass: OrderedCollection selector: #add:;
		addClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment selectorsForClass: Object) size = 1.
	self assert: (environment includesClass: OrderedCollection).
	self assert: (environment selectorsForClass: OrderedCollection) size = 1.
	self assert: (environment includesClass: Collection).
	self assert: (environment selectorsForClass: Collection) size 
				= Collection selectors size.
	self deny: (environment includesClass: Object class).
	newEnvironment := environment copy.
	newEnvironment
		removeClass: OrderedCollection;
		addClass: Object selector: #printOn:;
		removeClass: Object selector: #printString.
	self assert: (newEnvironment includesClass: Object).
	self deny: (newEnvironment includesSelector: #printString in: Object).
	self deny: (newEnvironment includesClass: OrderedCollection).
	self assert: (newEnvironment includesClass: Collection).
	self assert: newEnvironment numberClasses = 2.
	self 
		assert: newEnvironment numberSelectors + 1 = environment numberSelectors.
	newEnvironment addClass: Object class.
	self assert: newEnvironment numberClasses = 2.
	self assert: (newEnvironment includesClass: Object class).
	newEnvironment removeClass: self class.
	self assert: newEnvironment numberClasses = 2