tests
testClasses
	| classes |
	classes := STON listClass withAll: { Point. Integer. Object }.
	self serializeAndMaterialize: classes.
	classes := STON listClass withAll: { Point class. Integer class. Object class }.
	self serializeAndMaterialize: classes.
	classes := STON listClass withAll: { Class. Metaclass. Class class. Point class class }.
	self serializeAndMaterialize: classes.