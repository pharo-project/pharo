Extension { #name : 'RangeSettingDeclaration' }

{ #category : '*System-Settings-Browser' }
RangeSettingDeclaration >> hasEditableList [
	^ false
]

{ #category : '*System-Settings-Browser' }
RangeSettingDeclaration >> inputWidget [
	^ (self theme
			newRowIn: self
			for: {
				(self theme
					newTextEntryIn: self
					for: self
					get: #realValue
					set: #realValue:
					class: (Smalltalk globals at: self type)
					getEnabled: #enabled
					help: nil)
						hResizing: #rigid;
						width: 80;
						wantsFrameAdornments: true;
						yourself.
				(PluggableSliderMorph new max: self range last;
					 min: self range first;
					 quantum: self range increment;
					on: self
					getValue: #realValue
					setValue: #realValue:) hResizing: #spaceFill;
					 vResizing: #spaceFIll;
					 setBalloonText: self description;
					 minWidth: 300; height: 20;
					 yourself})
		cellInset: 10;
		yourself
]
