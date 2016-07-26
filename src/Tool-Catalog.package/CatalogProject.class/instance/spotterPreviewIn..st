gt-spotter-extension
spotterPreviewIn: aComposite
	<spotterPreview: 10>
	^ aComposite table
		title: 'Catalog Project';
		display: [ {
			'name' -> self name.
			'package name' -> self packageName.
			'repository url' -> self repositoryUrl.
			'description' -> (self description ifNil: [ '' ]).
			'contact info' -> (self contactInfo ifNil: [ '' ]).
			'keywords' -> (Character space join: self keywords) } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		entity: self