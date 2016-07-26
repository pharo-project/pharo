private
name: name key: key class: leafNodeClass type: type set: dict 
	^dict at: key
		ifAbsentPut: 
			[leafNodeClass new 
				name: name
				key: key
				index: nil
				type: type]