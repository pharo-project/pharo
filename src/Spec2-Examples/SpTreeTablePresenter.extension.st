Extension { #name : #SpTreeTablePresenter }

{ #category : #'*Spec2-Examples' }
SpTreeTablePresenter class >> example [
	<sampleInstance>
	^ self new
		addColumn:
			(SpCompositeTableColumn new
				title: 'Classes';
				addColumn:
					(SpImageTableColumn new
						width: 20;
						evaluated: #systemIcon;
						yourself);
				addColumn:
					(SpStringTableColumn new
						evaluated: #name;
						yourself);
				yourself);
		roots: {Object};
		children: [ :aClass | aClass subclasses ];
		openWithSpec
]
