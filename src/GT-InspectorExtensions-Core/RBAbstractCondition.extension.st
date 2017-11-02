Extension { #name : #RBAbstractCondition }

{ #category : #'*GT-InspectorExtensions-Core' }
RBAbstractCondition class >> gtExampleBasic [
	<gtExample>
	^ RBCondition new
		type: { }
		block: [ true ]
		errorString: #'error'
]

{ #category : #'*GT-InspectorExtensions-Core' }
RBAbstractCondition >> gtInspectorChildren [
	^ #()
]

{ #category : #'*GT-InspectorExtensions-Core' }
RBAbstractCondition >> gtInspectorTreeIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite tree
		title: 'Tree';
		display: [ { self } ]	;
		children: [ :each | each gtInspectorChildren ];
		when: [ self gtInspectorChildren notEmpty ]
]
