specs
defaultSpec
	<spec>
	^ SpecLayout composed
		newRow:
				[ :row | 
			row
				add: #(#model #browseModel);
				add: #(#model #usersModel);
				add: #(#model #sendersModel);
				add: #(#model #implementorsModel);
				add: #(#model #versionModel);
				add: #(#model #dropListModel) ];
		yourself