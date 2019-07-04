Extension { #name : #TreeTablePresenter }

{ #category : #'*Spec-Examples' }
TreeTablePresenter class >> example [
	<sampleInstance>
	^ self new
		addColumn:
			(CompositeTableColumn new
				title: 'Classes';
				addColumn:
					(ImageTableColumn new
						width: 20;
						evaluated: #systemIcon;
						yourself);
				addColumn:
					(StringTableColumn new
						evaluated: #name;
						yourself);
				yourself);
		roots: {Object};
		children: [ :aClass | aClass subclasses ];
		openWithSpec
]
