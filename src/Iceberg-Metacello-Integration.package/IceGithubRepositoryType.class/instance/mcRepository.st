accessing
mcRepository
	| baseRepo |
	
	baseRepo := MCGitHubRepository location: self location.
	(Iceberg icebergRepositoriesURLs includes: baseRepo scpUrl)
		ifTrue: [ 
			"Do not use Iceberg to load iceberg code, 
			see https://github.com/npasserini/iceberg/issues/168"
			^ baseRepo ]
		ifFalse: [  
			Transcript 
				show: 'Creating iceberg-metacello adapter for: ';
				show: self location; 
				cr.
			^ baseRepo getOrCreateIcebergRepository metacelloAdapter ]
		