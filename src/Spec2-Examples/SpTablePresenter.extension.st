Extension { #name : #SpTablePresenter }

{ #category : #'*Spec2-Examples' }
SpTablePresenter class >> example [
	<sampleInstance>
	
	^ self new
		addColumn: (SpImageTableColumn title: 'Icon' evaluated: #systemIcon);
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
		items: Smalltalk allClasses;
		doubleClickAction: [ :aClass | self inform: 'Double click on: ', aClass name ];
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpTablePresenter class >> exampleEditableCell [

	self flag: #TODO
]

{ #category : #'*Spec2-Examples' }
SpTablePresenter class >> exampleWithColumnHeaders [
	<sampleInstance>
	| example |
	example := self new.
	example
		addColumn: (SpStringTableColumn title: 'Number' evaluated: #yourself);
		addColumn: (SpStringTableColumn title: 'Hex' evaluated: #hex);
		showColumnHeaders;
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #'*Spec2-Examples' }
SpTablePresenter class >> exampleWithoutColumnHeaders [
	<sampleInstance>
	| example |
	example := self new.
	example
		addColumn: (SpStringTableColumn title: 'Number' evaluated: #yourself);
		addColumn: (SpStringTableColumn title: 'Hex' evaluated: #hex);
		items: (1 to: 10) asArray;
		hideColumnHeaders;
		openWithSpec.
	^ example
]
