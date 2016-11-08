others
compoundTaggedTree
	"self new compoundTaggedTree openOn: #(window help home)"

	<glmBrowser: 'Tags' input: '#(window help home)'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a tree
				title: 'Combined Tags';
				tags: [ :each | 
					{{(self iconNamed: each asSymbol).
					each asString}} ].
			a tree
				title: 'Image Tags';
				tags: [ :each | {(self iconNamed: each asSymbol)} ].
			a tree
				title: 'Combined Tags - Filter by image';
				tags: [ :each | 
					{(each == #help
						ifTrue: [ {(self iconNamed: #smallDeleteIcon).
							'no ok'} ]
						ifFalse: [ {(self iconNamed: #smallOkIcon).
							each asString} ])} ];
				tagsFilter: [ :each | 
					{(each == #help
						ifTrue: [ self iconNamed: #smallDeleteIcon ]
						ifFalse: [ self iconNamed: #smallOkIcon ])} ].
			a tree
				title: 'Combined Tags - Filter by label';
				tags: [ :each | 
					{{(self iconNamed: each asSymbol).
					(each == #help
						ifTrue: [ 'no ok' ]
						ifFalse: [ 'ok' ])}} ];
				tagsFilter: [ :each | 
					{(each == #help
						ifTrue: [ 'no ok' ]
						ifFalse: [ 'ok' ])} ] ].
	^ browser