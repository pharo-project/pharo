tests
testBehaviorLocalyDefined
	"method not acquired from a trait or from its superclass"
	" self debug: #testBehavior "
	self assert: (Object methodDict isKindOf: MethodDictionary).
	self assert: (Object selectors asSortedCollection = Object selectors asSortedCollection).