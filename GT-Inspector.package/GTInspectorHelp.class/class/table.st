documentation
table
	^ HelpTopic 
		title: 'Table'
		contents: 
'A table is a presentation that simply list elements. For example, you can create a table presentation for the class OrderedCollection by defining the following method in OrderedCollection:
-=-=-=-=
gtInspectorTableIn: composite
	<gtInspectorPresentationOrder: -15>
	composite table
		title: ''Table'';
		display: [ self collect: #asString ]
-=-=-=-=

Inspect the expression:
((1 to: 5), (1 to: 5), (1 to: 5)) asOrderedCollection

You will have a list of numbers.

'