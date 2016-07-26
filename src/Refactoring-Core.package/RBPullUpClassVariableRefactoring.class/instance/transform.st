transforming
transform
	| subclass |
	subclass := self subclassDefiningVariable.
	subclass removeClassVariable: variableName.
	class addClassVariable: variableName