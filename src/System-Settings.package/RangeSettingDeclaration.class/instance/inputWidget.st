user interface
inputWidget
	^ (self theme newRowIn: World for: {(self theme 
							newTextEntryIn: World 
							for: self
							get: #realValue
							set: #realValue:
							class: (Smalltalk globals at: self type)
							getEnabled: #enabled
							help: nil) hResizing: #rigid;
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