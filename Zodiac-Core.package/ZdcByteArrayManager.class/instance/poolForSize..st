private
poolForSize: size
	^ self resources at: size ifAbsentPut: [ Array new: limit ]