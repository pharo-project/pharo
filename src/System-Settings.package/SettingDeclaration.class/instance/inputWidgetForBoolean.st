user interface
inputWidgetForBoolean
	^  self theme  
			newCheckboxIn: World
			for: self
			getSelected: #realValue
			setSelected: #realValue:
			getEnabled: #enabled
			label: '' 
			help: nil