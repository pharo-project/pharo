Extension { #name : #TablePresenter }

{ #category : #'*Spec-Examples' }
TablePresenter class >> example [
	<sampleInstance>
	
	^ self new
		addColumn: (ImageTableColumn title: 'Icon' evaluated: #systemIcon);
		addColumn: (StringTableColumn title: 'Name' evaluated: #name);
		items: Smalltalk allClasses;
		doubleClickAction: [ :aClass | self inform: 'Double click on: ', aClass name ];
		openWithSpec
]

{ #category : #'*Spec-Examples' }
TablePresenter class >> exampleEditableCell [

	self flag: #TODO
]

{ #category : #'*Spec-Examples' }
TablePresenter class >> exampleWithColumnHeaders [
	<sampleInstance>
	| example |
	example := self new.
	example
		addColumn: (StringTableColumn title: 'Number' evaluated: #yourself);
		addColumn: (StringTableColumn title: 'Hex' evaluated: #hex);
		showColumnHeaders;
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #'*Spec-Examples' }
TablePresenter class >> exampleWithoutColumnHeaders [
	<sampleInstance>
	| example |
	example := self new.
	example
		addColumn: (StringTableColumn title: 'Number' evaluated: #yourself);
		addColumn: (StringTableColumn title: 'Hex' evaluated: #hex);
		items: (1 to: 10) asArray;
		hideColumnHeaders;
		openWithSpec.
	^ example
]
