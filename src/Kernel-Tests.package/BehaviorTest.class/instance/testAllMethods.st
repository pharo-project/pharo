tests
testAllMethods
	| allMethods nonOverridenMethods |
	allMethods := IdentitySet new
		addAll: Object allMethods;
		yourself.
	self assert: (allMethods includesAll: Object methods).	"We fetch all compiled methods that are not redefined in Object"
	nonOverridenMethods := OrderedCollection new.
	ProtoObject
		selectorsAndMethodsDo: [ :sel :method | 
			(Object includesSelector: sel)
				ifFalse: [ nonOverridenMethods add: method ] ].
	self assert: (allMethods includesAll: nonOverridenMethods)