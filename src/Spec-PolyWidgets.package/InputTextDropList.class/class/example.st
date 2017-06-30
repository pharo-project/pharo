example
example
	| ex |
	ex := self new.
	ex
		ghostText: 'a Number';
		displayBlock: [ :item | item asString ];
		items: {'Potatoes' . 'Carrots' . 'Onions'};
		extent: 350 @ 50;
		title: 'InputTextDropList example';
		openWithSpec.
	^ ex