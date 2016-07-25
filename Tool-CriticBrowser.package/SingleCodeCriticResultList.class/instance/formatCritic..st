private
formatCritic: aCritic
	
	^  String streamContents: [:s | aCritic sourceAnchor entity criticNameOn: s ]
