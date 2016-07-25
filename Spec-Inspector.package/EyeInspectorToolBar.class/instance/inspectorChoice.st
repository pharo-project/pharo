accessing
inspectorChoice
	inspectorChoice
		ifNil: [ 
			inspectorChoice := self instantiate: DropListModel.
			inspectorChoice
				whenSelectedItemChanged: [ :inspectorType |
					self preventUpdatesDuring: [
						self inspectorType: inspectorType ]]].
	^ inspectorChoice