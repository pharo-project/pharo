building
buildBrowser
	| browser |
	browser := GLMTabulator new
		title: self class title;
		row: #projects;
		row: #detail;
		yourself.
	browser transmit
		to: #projects;
		andShow:
				[ :a | 
			a list
				title: 'Available';
				in: [ :this | 
					self buildMenuOn: this.
					self buildContextMenuOn: this ];
				display: [ :thisProvider | thisProvider projects ];
				format: [ :project | project name ];
				icon: [ :project | self iconFor: project ];
				filterOn: [ :text :each | each matches: text ];
				tags: [ :project | 
					project keywords isNil
						ifTrue: [ #() ]
						ifFalse: [ project allKeywords sorted ] ] ].
	browser transmit
		from: #projects;
		to: #detail;
		andShow: [ :a | 
			a text
				title: 'Description';
				display: [ :project | project fullDescription ] ].
	^ browser