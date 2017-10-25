initialization
initialize

	super initialize.
	max := 100 asValueHolder.
	min := 0 asValueHolder.
	quantum := 1 asValueHolder.
	value := 0 asValueHolder.
	absoluteValue := 0 asValueHolder.
	labelHolder := '' asValueHolder.
	isChanging := false.
	
	max whenChangedDo: [:m | self changed: { #max: . m } ].
	min whenChangedDo: [:m | self changed: { #min: . m } ].
	quantum whenChangedDo: [:m | self changed: { #quantum: . m } ].
	
	value whenChangedDo: [:v |
		isChanging ifFalse: [
			isChanging := true.
			self absoluteValue: ((v-self min)/(self max-self min)).
			self changed: { #scaledValue: . v }.
			isChanging := false ].
		self changed: #value.].
	
	absoluteValue whenChangedDo: [:v | 
		isChanging ifFalse: [
			isChanging := true.
			self value: (self min+(v*(self max-self min)) roundTo: self quantum).
			self changed: { #value: . v }.
			isChanging := false ]].
	
	labelHolder whenChangedDo: [:v | self changed: #label ].