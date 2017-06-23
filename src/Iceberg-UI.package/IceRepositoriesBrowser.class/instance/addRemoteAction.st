menu actions - selected repository
addRemoteAction
	^ GLMGenericAction new
		action: [ :presenter | 
			presenter selection addRemoteThen: [ :remote | 
				self announce: (IceRemoteAdded remote: remote) ] ];
		selectionCondition: #isOperative;
		showTitle: 'Add remote...'; 
		yourself
