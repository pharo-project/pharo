accessing
inspectorChoice
	inspectorChoice
		ifNil: [ 
			inspectorChoice := self instantiate: DropListPresenter.
			inspectorChoice
				whenSelectedItemChanged: [ :inspectorType |
					self preventUpdatesDuring: [
						self inspectorType: inspectorType ]]].
	^ inspectorChoice