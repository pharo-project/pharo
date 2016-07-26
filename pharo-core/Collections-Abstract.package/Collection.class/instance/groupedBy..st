enumerating
groupedBy: aBlock
	"Answer a dictionary whose keys are the result of evaluating aBlock
	for all my elements, and the value for each key is the selection of 
	my elements that evaluated to that key. Uses species."
	| groups |
	groups := PluggableDictionary integerDictionary.
	self do: [ :each |
		(groups at: (aBlock value: each) ifAbsentPut: [ OrderedCollection new ]) add: each ].
	self species ~~ OrderedCollection ifTrue: [
		groups associationsDo: [ :association |
			association value: (self species withAll: association value) ]].
	^ groups
