building
addChangesTreesTo: browser
	browser transmit 
		from: #commits;
		to: #changes; 
		andShow: [ :a :commitInfo |
			(IceDiffChangeTreeBuilder new 
				entity: commitInfo;
				diff: (commitInfo ifNotNil: [ commitInfo diffFromHead ]); 
				buildOn: a)
				title: 'Changes since selection'.
			(IceDiffChangeTreeBuilder new
				entity: commitInfo;
				diff: (commitInfo ifNotNil: [ commitInfo diffToParent ]);
				buildOn: a)
				title: 'Changes vs. parent' ].