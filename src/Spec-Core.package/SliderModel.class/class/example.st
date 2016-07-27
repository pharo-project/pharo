specs
example
	| slider |
	slider := self new.
	slider
		min: 0;
		max: 100;
		whenValueChangedDo: [ :value | slider label: value asString ].
	slider openWithSpec.
	^ slider 