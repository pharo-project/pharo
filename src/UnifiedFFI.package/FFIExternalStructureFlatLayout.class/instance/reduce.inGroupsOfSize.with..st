system v abi
reduce: sourceFields inGroupsOfSize: groupSize with: aBlock
	| reduced nextGroupOffset |
	reduced := nil.
	nextGroupOffset := groupSize.
	sourceFields do: [ :field |
		field offset < nextGroupOffset ifFalse: [ 
			reduced ifNotNil: [
				fields add: reduced.
				reduced := nil
			].
			nextGroupOffset := (field offset // groupSize + 1) * groupSize
		].
	
		reduced ifNotNil: [
			reduced
				registerClass: (aBlock value: reduced value: field);
				size: (reduced size alignedTo: field alignment) + field size;
				alignment: (reduced alignment max: field alignment)
		] ifNil: [
			reduced := field copy
		]
	].

	reduced ifNotNil: [ fields add: reduced ]
