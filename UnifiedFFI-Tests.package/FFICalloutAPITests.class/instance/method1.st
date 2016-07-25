accessing
method1 
	| object index |
	object := FFICalloutObjectForTest new.
	index := FFICalloutObjectForTest class instVarIndexFor: #handle ifAbsent: [ nil ].
	^ object instVarAt: index.