building
buildBrowser
	| browser |
	browser := GLMTabulator new
		title: [ self title ];
		row: #actions;
		row: #comment;
		row: #oldComment size: 35;
		yourself.
	browser transmit
		to: #actions;
		andShow: [ :a | 
			a table
				title: 'Dirty packages';
				display: [ self unit allActions ];
				act: [ :presenter | 
					self configure.
					presenter pane browser update ]
					icon: (self iconNamed: #smallConfigurationIcon)
					entitled: 'Configure commit options';
				act: [ :presenter | 
					self refresh.
					presenter pane browser update ]
					icon: (self iconNamed: #glamorousRefresh)
					entitled: 'Refresh';
				act: [ :presenter | 
					self commit.
					presenter pane browser update ]
					icon: (self iconNamed: #smallDoItIcon)
					entitled: 'Commit';
				selectionAct: [ :presenter | self actionBrowse: presenter selection ]
					on: $b
					entitled: 'Browse';
				selectionAct: [ :presenter | 
					self actionRemove: presenter selection.
					presenter update ]
					entitled: 'Remove from this release/commit';
				icon: [ :action | action icon ];
				column: 'Name'
					evaluated: [ :action | ' ' , action displayString ];
				column: 'Version' evaluated: [ :action | action currentString ];
				column: 'Next'
					evaluated: [ :action | (action nextVersionOn: self unit) ifNil: [ '' ] ]
					modified: [ :value :action :presenter | 
						[ self action: action nextVersion: value ]
							on: Error
							do: [ :e | 
								self inform: e messageText.
								presenter pane browser update ] ]
					width: 80;
				column: 'Repository' evaluated: [ :action | action repository ] ].
	browser transmit
		to: #comment;
		andShow: [ :a | self buildCommentOn: a ].
	browser transmit
		to: #oldComment;
		andShow: [ :a | a dropDownList display: [ self oldComments ] ].
	browser transmit
		from: #oldComment;
		to: #comment;
		andShow: [ :a | self buildCommentOn: a ].
	^ browser