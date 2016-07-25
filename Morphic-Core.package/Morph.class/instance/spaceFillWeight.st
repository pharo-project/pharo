layout-properties
spaceFillWeight
	"Layout specific. This property describes the relative weight that 
	should be given to the receiver when extra space is distributed 
	between different #spaceFill cells."

	^ self
		valueOfProperty: #spaceFillWeight
		ifAbsent: [1]