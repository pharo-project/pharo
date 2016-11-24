*GT-InspectorExtensions-Core
gtInspectorInstanceVariablesIn: composite
	"This provides a list of all class instance variables and the corresponding values"

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	<gtInspectorPresentationOrder: 21>
	composite table
		title: 'InstVars';
		display: [ self class allInstVarNames sorted collect: [ :each | each -> (self instVarNamed: each) ] ];
		column: 'Variable' evaluated: [ :each | each key ];
		column: 'Value' evaluated: [ :each | each value ]