Extension { #name : #IceNode }

{ #category : #'*Iceberg-TipUI' }
IceNode >> gtInspectorItemsIn: composite [
	<gtInspectorPresentationOrder: 0>
	^ composite fastTable 
		title: 'Items';
		display: [ self children ];
		column: 'Key' evaluated: [:each | each key ] width: 200 * World displayScaleFactor;
		column: 'Value' evaluated: [:each | each value ] width: 400 * World displayScaleFactor
]
