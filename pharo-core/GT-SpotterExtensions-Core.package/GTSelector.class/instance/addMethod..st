private accessing
addMethod: aCompiledMethod
	"this is a private method that can be used by the constructor on the class side
	exclusively within the context of Spotter"
	methods ifNil: [ methods := OrderedCollection new ].
	methods add: aCompiledMethod