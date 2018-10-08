Extension { #name : #SequenceableCollection }

{ #category : #'*GT-InspectorExtensions-Core' }
SequenceableCollection >> gtInspectorItemsIn: composite [
	^ composite fastTable
		title: 'Items';
		display: [ self ];
		beMultiple;
		enableElementIndex;
		wantsAutomaticRefresh: true;
		column: 'Index' evaluated: [ :value :idex | idex asString ] width: 50;
		column: 'Item' evaluated: [ :value | GTObjectPrinter asTruncatedTextFrom: value ]
]
