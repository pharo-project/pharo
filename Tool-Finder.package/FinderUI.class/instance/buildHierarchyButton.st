items creation
buildHierarchyButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #hierarchyButtonState 
			action: #hierarchyButtonAction 
			label: #hierarchyButtonLabel)
			hResizing: #spaceFill