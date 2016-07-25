instance creation
named: label do: aBlock

	^ self new
		action: aBlock;
		model: label;
		display: [ :e | e ];
		yourself